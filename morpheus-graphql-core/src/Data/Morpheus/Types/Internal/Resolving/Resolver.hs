{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Resolving.Resolver
  ( Event (..),
    Resolver,
    LiftOperation,
    toResolver,
    lift,
    subscribe,
    SubEvent,
    GQLChannel (..),
    ResponseEvent (..),
    ResponseStream,
    ObjectResModel (..),
    ResModel (..),
    FieldResModel,
    WithOperation,
    Context (..),
    unsafeInternalContext,
    runRootResModel,
    setTypeName,
    RootResModel (..),
    liftStateless,
    withArguments,
    SubscriptionField (..),
    ChannelOf,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), join)
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.IO.Class (MonadIO (..))
-- MORPHEUS

import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
    mapReaderT,
  )
import Data.Functor ((<$>), Functor (..))
import Data.Maybe (Maybe (..), maybe)
import Data.Morpheus.Error.Internal (internalResolvingError)
import Data.Morpheus.Error.Selection (subfieldsNotSelected)
import Data.Morpheus.Internal.Utils
  ( Merge (..),
    elems,
    empty,
    keyOf,
    selectOr,
  )
import Data.Morpheus.Types.IO
  ( GQLResponse,
    renderResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( Arguments,
    FieldName,
    GQLError (..),
    GQLErrors,
    GQLValue (..),
    MUTATION,
    Message,
    ObjectEntry (..),
    Operation (..),
    OperationType,
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    ScalarValue (..),
    Schema,
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeName (..),
    UnionSelection,
    UnionTag (..),
    VALID,
    ValidValue,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.AST.MergeSet
  ( toOrdMap,
  )
import Data.Morpheus.Types.Internal.Resolving.Core
  ( Channel (..),
    Event (..),
    Eventless,
    Failure (..),
    GQLChannel (..),
    PushEvents (..),
    Result (..),
    ResultT (..),
    StreamChannel,
    cleanEvents,
    mapEvent,
    statelessToResultT,
  )
import Data.Semigroup
  ( Semigroup (..),
  )
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (.),
    Eq (..),
    Show (..),
    concatMap,
    const,
    lookup,
    otherwise,
  )

type WithOperation (o :: OperationType) = LiftOperation o

type ResponseStream event (m :: * -> *) = ResultT (ResponseEvent event m) m

data ResponseEvent event (m :: * -> *)
  = Publish event
  | Subscribe (SubEvent event m)

type SubEvent event m = Event (Channel event) (event -> m GQLResponse)

data SubscriptionField a = SubscriptionField
  { channel :: ChannelOf a,
    unSubscribe :: a
  }

-- Converts Subscription Resolver Type to Query Resolver
type family ChannelOf (a :: *) :: *

type instance ChannelOf (Resolver SUBSCRIPTION e m a) = StreamChannel e

type instance ChannelOf (a -> Resolver SUBSCRIPTION e m b) = StreamChannel e

-- | A datatype to expose 'Schema' and the query's AST information ('Selection', 'Operation').
data Context = Context
  { currentSelection :: Selection VALID,
    schema :: Schema,
    operation :: Operation VALID,
    currentTypeName :: TypeName
  }
  deriving (Show)

-- Resolver Internal State
newtype ResolverState event m a = ResolverState
  { runResolverState :: ReaderT Context (ResultT event m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Context
    )

instance MonadTrans (ResolverState e) where
  lift = ResolverState . lift . lift

instance (Monad m) => Failure Message (ResolverState e m) where
  failure message = ResolverState $ do
    selection <- asks currentSelection
    lift $ failure [resolverFailureMessage selection message]

instance (Monad m) => Failure GQLErrors (ResolverState e m) where
  failure = ResolverState . lift . failure

instance (Monad m) => PushEvents e (ResolverState e m) where
  pushEvents = ResolverState . lift . pushEvents

mapResolverState ::
  ( ResultT e m a ->
    ResultT e' m' a'
  ) ->
  ResolverState e m a ->
  ResolverState e' m' a'
mapResolverState f (ResolverState x) = ResolverState (mapReaderT f x)

-- clear evets and starts new resolver with diferenct type of events but with same value
-- use properly. only if you know what you are doing
clearStateResolverEvents :: (Functor m) => ResolverState e m a -> ResolverState e' m a
clearStateResolverEvents = mapResolverState cleanEvents

resolverFailureMessage :: Selection VALID -> Message -> GQLError
resolverFailureMessage Selection {selectionName, selectionPosition} message =
  GQLError
    { message = "Failure on Resolving Field " <> msg selectionName <> ": " <> message,
      locations = [selectionPosition]
    }

--
-- GraphQL Field Resolver
--
---------------------------------------------------------------
data Resolver (o :: OperationType) event (m :: * -> *) value where
  ResolverQ :: {runResolverQ :: ResolverState () m value} -> Resolver QUERY event m value
  ResolverM :: {runResolverM :: ResolverState event m value} -> Resolver MUTATION event m value
  ResolverS :: {runResolverS :: ResolverState () m (SubEventRes event m value)} -> Resolver SUBSCRIPTION event m value

type SubEventRes event m value = ReaderT event (ResolverState () m) value

instance Show (Resolver o e m value) where
  show ResolverQ {} = "Resolver QUERY e m a"
  show ResolverM {} = "Resolver MUTATION e m a"
  show ResolverS {} = "Resolver SUBSCRIPTION e m a"

deriving instance (Functor m) => Functor (Resolver o e m)

-- Applicative
instance (LiftOperation o, Monad m) => Applicative (Resolver o e m) where
  pure = packResolver . pure
  ResolverQ r1 <*> ResolverQ r2 = ResolverQ $ r1 <*> r2
  ResolverM r1 <*> ResolverM r2 = ResolverM $ r1 <*> r2
  ResolverS r1 <*> ResolverS r2 = ResolverS $ (<*>) <$> r1 <*> r2

-- Monad
instance (Monad m, LiftOperation o) => Monad (Resolver o e m) where
  return = pure
  (>>=) = monadBind

#if __GLASGOW_HASKELL__ < 808
  fail = failure . msg
# endif

-- MonadIO
instance (MonadIO m, LiftOperation o) => MonadIO (Resolver o e m) where
  liftIO = lift . liftIO

-- Monad Transformers
instance (LiftOperation o) => MonadTrans (Resolver o e) where
  lift = packResolver . lift

-- Failure
instance (LiftOperation o, Monad m) => Failure Message (Resolver o e m) where
  failure = packResolver . failure

instance (LiftOperation o, Monad m) => Failure GQLErrors (Resolver o e m) where
  failure = packResolver . failure

instance (Monad m, LiftOperation o) => MonadFail (Resolver o e m) where
  fail = failure . msg

-- PushEvents
instance (Monad m) => PushEvents e (Resolver MUTATION e m) where
  pushEvents = packResolver . pushEvents

instance (Monad m, Semigroup a, LiftOperation o) => Semigroup (Resolver o e m a) where
  x <> y = fmap (<>) x <*> y

instance (LiftOperation o, Monad m) => MonadReader Context (Resolver o e m) where
  ask = packResolver ask
  local f (ResolverQ res) = ResolverQ (local f res)
  local f (ResolverM res) = ResolverM (local f res)
  local f (ResolverS resM) = ResolverS $ mapReaderT (local f) <$> resM

-- | A function to return the internal 'Context' within a resolver's monad.
-- Using the 'Context' itself is unsafe because it expposes internal structures
-- of the AST, but you can use the "Data.Morpheus.Types.SelectionTree" typeclass to manipulate
-- the internal AST with a safe interface.
unsafeInternalContext :: (Monad m, LiftOperation o) => Resolver o e m Context
unsafeInternalContext = ask

liftStateless ::
  ( LiftOperation o,
    Monad m
  ) =>
  Eventless a ->
  Resolver o e m a
liftStateless =
  packResolver
    . ResolverState
    . ReaderT
    . const
    . statelessToResultT

class LiftOperation (o :: OperationType) where
  packResolver :: Monad m => ResolverState e m a -> Resolver o e m a

instance LiftOperation QUERY where
  packResolver = ResolverQ . clearStateResolverEvents

instance LiftOperation MUTATION where
  packResolver = ResolverM

instance LiftOperation SUBSCRIPTION where
  packResolver = ResolverS . pure . lift . clearStateResolverEvents

setSelection :: (Monad m, LiftOperation o) => Selection VALID -> Resolver o e m a -> Resolver o e m a
setSelection currentSelection = local (\ctx -> ctx {currentSelection})

setTypeName :: (Monad m, LiftOperation o) => TypeName -> Resolver o e m a -> Resolver o e m a
setTypeName currentTypeName = local (\ctx -> ctx {currentTypeName})

monadBind ::
  forall o e m a b.
  Monad m =>
  Resolver o e m a ->
  (a -> Resolver o e m b) ->
  Resolver o e m b
monadBind (ResolverQ x) m2 = ResolverQ (x >>= runResolverQ . m2)
monadBind (ResolverM x) m2 = ResolverM (x >>= runResolverM . m2)
monadBind (ResolverS res) m2 = ResolverS $ do
  (readResA :: ReaderT e (ResolverState () m) a) <- res
  pure $ ReaderT $ \e -> do
    (a :: a) <- runReaderT readResA e
    (readResB :: ReaderT e (ResolverState () m) b) <- runResolverS (m2 a)
    runReaderT readResB e

subscribe ::
  forall e m a.
  ( PushEvents (Channel e) (ResolverState (Channel e) m),
    Monad m
  ) =>
  StreamChannel e ->
  Resolver QUERY e m (e -> Resolver SUBSCRIPTION e m a) ->
  SubscriptionField (Resolver SUBSCRIPTION e m a)
subscribe ch res =
  SubscriptionField ch
    $ ResolverS
    $ fixSub <$> runResolverQ res
  where
    fixSub :: (e -> Resolver SUBSCRIPTION e m a) -> ReaderT e (ResolverState () m) a
    fixSub f = join (ReaderT $ \e -> runResolverS (f e))

withArguments ::
  forall o e m a.
  (LiftOperation o, Monad m) =>
  (Arguments VALID -> Resolver o e m a) ->
  Resolver o e m a
withArguments = (getArguments >>=)

getArguments ::
  forall o e m a.
  (LiftOperation o, Monad m) =>
  Resolver o e m (Arguments VALID)
getArguments = selectionArguments . currentSelection <$> unsafeInternalContext

-- Selection Processing
toResolver ::
  forall o e m a b.
  (LiftOperation o, Monad m) =>
  (Arguments VALID -> Eventless a) ->
  (a -> Resolver o e m b) ->
  Resolver o e m b
toResolver toArgs resolver =
  getArguments >>= liftStateless . toArgs >>= resolver

pickSelection :: TypeName -> UnionSelection VALID -> SelectionSet VALID
pickSelection = selectOr empty unionTagSelection

withObject ::
  (LiftOperation o, Monad m) =>
  (SelectionSet VALID -> Resolver o e m value) ->
  Selection VALID ->
  Resolver o e m value
withObject f Selection {selectionName, selectionContent, selectionPosition} = checkContent selectionContent
  where
    checkContent (SelectionSet selection) = f selection
    checkContent _ = failure (subfieldsNotSelected selectionName "" selectionPosition)

lookupRes ::
  (LiftOperation o, Monad m) =>
  Selection VALID ->
  ObjectResModel o e m ->
  Resolver o e m ValidValue
lookupRes Selection {selectionName}
  | selectionName == "__typename" =
    pure . Scalar . String . readTypeName . __typename
  | otherwise =
    maybe
      (pure gqlNull)
      (>>= runDataResolver)
      . lookup selectionName
      . objectFields

resolveObject ::
  forall o e m.
  (LiftOperation o, Monad m) =>
  SelectionSet VALID ->
  ResModel o e m ->
  Resolver o e m ValidValue
resolveObject selectionSet (ResObject drv@ObjectResModel {__typename}) =
  Object . toOrdMap <$> traverse resolver selectionSet
  where
    resolver :: Selection VALID -> Resolver o e m (ObjectEntry VALID)
    resolver sel =
      setSelection sel
        $ setTypeName __typename
        $ ObjectEntry (keyOf sel) <$> lookupRes sel drv
resolveObject _ _ =
  failure $ internalResolvingError "expected object as resolver"

toEventResolver :: Monad m => SubEventRes event m ValidValue -> Context -> event -> m GQLResponse
toEventResolver (ReaderT subRes) sel event =
  renderResponse
    <$> runResultT (runReaderT (runResolverState $ subRes event) sel)

runDataResolver :: (Monad m, LiftOperation o) => ResModel o e m -> Resolver o e m ValidValue
runDataResolver res = asks currentSelection >>= __encode res
  where
    __encode obj sel@Selection {selectionContent} = encodeNode obj selectionContent
      where
        -- LIST
        encodeNode (ResList x) _ = List <$> traverse runDataResolver x
        -- Object -----------------
        encodeNode objDrv@ResObject {} _ = withObject (`resolveObject` objDrv) sel
        -- ENUM
        encodeNode (ResEnum _ enum) SelectionField = pure $ gqlString $ readTypeName enum
        encodeNode (ResEnum typename enum) unionSel@UnionSelection {} =
          encodeNode (unionDrv (typename <> "EnumObject")) unionSel
          where
            unionDrv name =
              ResUnion name
                $ pure
                $ ResObject
                $ ObjectResModel name [("enum", pure $ ResScalar $ String $ readTypeName enum)]
        encodeNode ResEnum {} _ =
          failure ("wrong selection on enum value" :: Message)
        -- UNION
        encodeNode (ResUnion typename unionRef) (UnionSelection selections) =
          unionRef >>= resolveObject currentSelection
          where
            currentSelection = pickSelection typename selections
        encodeNode (ResUnion name _) _ =
          failure ("union Resolver " <> msg name <> " should only recieve UnionSelection")
        -- SCALARS
        encodeNode ResNull _ = pure Null
        encodeNode (ResScalar x) SelectionField = pure $ Scalar x
        encodeNode ResScalar {} _ =
          failure ("scalar Resolver should only recieve SelectionField" :: Message)

runResolver ::
  Monad m =>
  [(FieldName, StreamChannel event)] ->
  Resolver o event m ValidValue ->
  Context ->
  ResponseStream event m ValidValue
runResolver _ (ResolverQ resT) sel = cleanEvents $ runReaderT (runResolverState resT) sel
runResolver _ (ResolverM resT) sel = mapEvent Publish $ runReaderT (runResolverState resT) sel
runResolver channels (ResolverS resT) ctx = ResultT $ do
  readResValue <- runResultT $ runReaderT (runResolverState resT) ctx
  pure $ case readResValue of
    Failure x -> Failure x
    Success {warnings, result} ->
      Success
        { events = subscriptionEvents (toEventResolver result ctx) ctx channels,
          warnings,
          result = gqlNull
        }

subscriptionEvents ::
  (e -> m GQLResponse) ->
  Context ->
  [(FieldName, StreamChannel e)] ->
  [ResponseEvent e m]
subscriptionEvents res ctx channels = [Subscribe (Event (channelBySelection ctx channels) res)]

channelBySelection :: Context -> [(FieldName, StreamChannel e)] -> [Channel e]
channelBySelection Context {currentSelection = Selection {selectionContent = SelectionSet selSet}} ch =
  concatMap getChannelFor (elems selSet)
  where
    getChannelFor Selection {selectionName} = case lookup selectionName ch of
      Nothing -> []
      Just x -> [Channel x]
channelBySelection _ _ = []

-- Resolver Models -------------------------------------------------------------------
type FieldResModel o e m =
  (FieldName, Resolver o e m (ResModel o e m))

data ObjectResModel o e m = ObjectResModel
  { __typename :: TypeName,
    objectFields ::
      [FieldResModel o e m]
  }
  deriving (Show)

instance Merge (ObjectResModel o e m) where
  merge _ (ObjectResModel tyname x) (ObjectResModel _ y) =
    pure $ ObjectResModel tyname (x <> y)

data ResModel (o :: OperationType) e (m :: * -> *)
  = ResNull
  | ResScalar ScalarValue
  | ResEnum TypeName TypeName
  | ResList [ResModel o e m]
  | ResObject (ObjectResModel o e m)
  | ResUnion TypeName (Resolver o e m (ResModel o e m))
  deriving (Show)

instance Merge (ResModel o e m) where
  merge p (ResObject x) (ResObject y) =
    ResObject <$> merge p x y
  merge _ _ _ =
    failure $ internalResolvingError "can't merge: incompatible resolvers"

data RootResModel e m = RootResModel
  { query :: Eventless (ResModel QUERY e m),
    mutation :: Eventless (ResModel MUTATION e m),
    subscription :: Eventless (ResModel SUBSCRIPTION e m),
    channelMap :: [(FieldName, StreamChannel e)]
  }

runRootDataResolver ::
  (Monad m, LiftOperation o) =>
  [(FieldName, StreamChannel e)] ->
  Eventless (ResModel o e m) ->
  Context ->
  ResponseStream e m (Value VALID)
runRootDataResolver
  channels
  res
  ctx@Context {operation = Operation {operationSelection}} =
    do
      root <- statelessToResultT res
      runResolver channels (resolveObject operationSelection root) ctx

runRootResModel :: Monad m => RootResModel e m -> Context -> ResponseStream e m (Value VALID)
runRootResModel
  RootResModel
    { query,
      mutation,
      subscription,
      channelMap
    }
  ctx@Context {operation = Operation {operationType}} =
    selectByOperation operationType
    where
      selectByOperation Query =
        runRootDataResolver [] query ctx
      selectByOperation Mutation =
        runRootDataResolver [] mutation ctx
      selectByOperation Subscription =
        runRootDataResolver channelMap subscription ctx
