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
    lift,
    subscribe,
    SubEvent,
    ResponseEvent (..),
    ResponseStream,
    ObjectResModel (..),
    ResModel (..),
    FieldResModel,
    WithOperation,
    Context (..),
    unsafeInternalContext,
    runRootResModel,
    RootResModel (..),
    liftStateless,
    withArguments,
    getArguments,
    SubscriptionField (..),
    liftResolverState,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), join)
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
    mapReaderT,
  )
import Data.Functor ((<$>), Functor (..))
import Data.Maybe (Maybe (..), maybe)
import Data.Morpheus.Error.Selection (subfieldsNotSelected)
import Data.Morpheus.Internal.Utils
  ( SemigroupM (..),
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
    GQLErrors,
    GQLValue (..),
    InternalError,
    MUTATION,
    Message,
    ObjectEntry (..),
    Operation (..),
    OperationType,
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    ScalarValue (..),
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
  ( Eventless,
    Failure (..),
    PushEvents (..),
    Result (..),
    ResultT (..),
    cleanEvents,
    mapEvent,
    statelessToResultT,
  )
import Data.Morpheus.Types.Internal.Resolving.Event
  ( Channel (..),
    Event (..),
  )
import Data.Morpheus.Types.Internal.Resolving.ResolverState
  ( Context (..),
    ResolverState,
    ResolverStateT (..),
    clearStateResolverEvents,
    resolverFailureMessage,
    runResolverState,
    runResolverStateM,
    runResolverStateT,
    toResolverStateT,
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

data SubscriptionField (a :: *) where
  SubscriptionField ::
    { channel :: forall e m v. a ~ Resolver SUBSCRIPTION e m v => Channel e,
      unSubscribe :: a
    } ->
    SubscriptionField a

--
-- GraphQL Field Resolver
--
---------------------------------------------------------------
data Resolver (o :: OperationType) event (m :: * -> *) value where
  ResolverQ :: {runResolverQ :: ResolverStateT () m value} -> Resolver QUERY event m value
  ResolverM :: {runResolverM :: ResolverStateT event m value} -> Resolver MUTATION event m value
  ResolverS :: {runResolverS :: ResolverStateT () m (SubEventRes event m value)} -> Resolver SUBSCRIPTION event m value

type SubEventRes event m value = ReaderT event (ResolverStateT () m) value

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
  (ResolverQ x) >>= m2 = ResolverQ (x >>= runResolverQ . m2)
  (ResolverM x) >>= m2 = ResolverM (x >>= runResolverM . m2)
  (ResolverS res) >>= m2 = ResolverS (liftSubResolver m2 <$> res)

#if __GLASGOW_HASKELL__ < 808
  fail = failure . msg
# endif

liftSubResolver ::
  (Monad m) =>
  (t -> Resolver SUBSCRIPTION r m a) ->
  ReaderT r (ResolverStateT () m) t ->
  ReaderT r (ResolverStateT () m) a
liftSubResolver m2 readResA = ReaderT $ \e -> do
  a <- runReaderT readResA e
  readResB <- runResolverS (m2 a)
  runReaderT readResB e

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
    . ResolverStateT
    . ReaderT
    . const
    . statelessToResultT

liftResolverState :: (LiftOperation o, Monad m) => ResolverState a -> Resolver o e m a
liftResolverState = packResolver . toResolverStateT

class LiftOperation (o :: OperationType) where
  packResolver :: Monad m => ResolverStateT e m a -> Resolver o e m a

instance LiftOperation QUERY where
  packResolver = ResolverQ . clearStateResolverEvents

instance LiftOperation MUTATION where
  packResolver = ResolverM

instance LiftOperation SUBSCRIPTION where
  packResolver = ResolverS . pure . lift . clearStateResolverEvents

subscribe ::
  forall e channel cont m a.
  (Monad m, Event channel cont ~ e) =>
  channel ->
  Resolver QUERY e m (e -> Resolver SUBSCRIPTION e m a) ->
  SubscriptionField (Resolver SUBSCRIPTION e m a)
subscribe ch res =
  SubscriptionField (Channel ch)
    $ ResolverS
    $ fromSub <$> runResolverQ res
  where
    fromSub :: (e -> Resolver SUBSCRIPTION e m a) -> ReaderT e (ResolverStateT () m) a
    fromSub f = join (ReaderT $ \e -> runResolverS (f e))

withArguments ::
  (LiftOperation o, Monad m) =>
  (Arguments VALID -> Resolver o e m a) ->
  Resolver o e m a
withArguments = (getArguments >>=)

getArguments ::
  (LiftOperation o, Monad m) =>
  Resolver o e m (Arguments VALID)
getArguments = selectionArguments . currentSelection <$> unsafeInternalContext

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
    resolver currentSelection =
      local (\ctx -> ctx {currentSelection, currentTypeName = __typename}) $
        ObjectEntry (keyOf currentSelection) <$> lookupRes currentSelection drv
resolveObject _ _ = packResolver $ failure ("expected object as resolver" :: InternalError)

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
  Maybe (Selection VALID -> ResolverState (Channel event)) ->
  Resolver o event m ValidValue ->
  Context ->
  ResponseStream event m ValidValue
runResolver _ (ResolverQ resT) sel = cleanEvents $ runResolverStateT resT sel
runResolver _ (ResolverM resT) sel = mapEvent Publish $ runResolverStateT resT sel
runResolver toChannel (ResolverS resT) ctx = ResultT $ do
  readResValue <- runResolverStateM resT ctx
  pure $ case readResValue >>= subscriptionEvents ctx toChannel . toEventResolver ctx of
    Failure x -> Failure x
    Success {warnings, result} ->
      Success
        { events = [result],
          warnings,
          result = gqlNull
        }

toEventResolver :: Monad m => Context -> SubEventRes event m ValidValue -> (event -> m GQLResponse)
toEventResolver sel (ReaderT subRes) event = renderResponse <$> runResolverStateM (subRes event) sel

subscriptionEvents ::
  Context ->
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
  (e -> m GQLResponse) ->
  Eventless (ResponseEvent e m)
subscriptionEvents ctx@Context {currentSelection} (Just channelGenerator) res =
  runResolverState handle ctx
  where
    handle = do
      channel <- channelGenerator currentSelection
      pure $ Subscribe (Event [channel] res)
subscriptionEvents Context {currentSelection} Nothing _ =
  failure [resolverFailureMessage currentSelection "channel Resolver is not defined"]

-- Resolver Models -------------------------------------------------------------------
type FieldResModel o e m =
  (FieldName, Resolver o e m (ResModel o e m))

data ObjectResModel o e m = ObjectResModel
  { __typename :: TypeName,
    objectFields ::
      [FieldResModel o e m]
  }
  deriving (Show)

instance Applicative f => SemigroupM f (ObjectResModel o e m) where
  sjoin _ (ObjectResModel tyname x) (ObjectResModel _ y) =
    pure $ ObjectResModel tyname (x <> y)

data ResModel (o :: OperationType) e (m :: * -> *)
  = ResNull
  | ResScalar ScalarValue
  | ResEnum TypeName TypeName
  | ResList [ResModel o e m]
  | ResObject (ObjectResModel o e m)
  | ResUnion TypeName (Resolver o e m (ResModel o e m))
  deriving (Show)

instance (Monad f, Failure InternalError f) => SemigroupM f (ResModel o e m) where
  sjoin p (ResObject x) (ResObject y) =
    ResObject <$> sjoin p x y
  sjoin _ _ _ = failure ("can't merge: incompatible resolvers" :: InternalError)

data RootResModel e m = RootResModel
  { query :: Eventless (ResModel QUERY e m),
    mutation :: Eventless (ResModel MUTATION e m),
    subscription :: Eventless (ResModel SUBSCRIPTION e m),
    channelMap :: Maybe (Selection VALID -> ResolverState (Channel e))
  }

runRootDataResolver ::
  (Monad m, LiftOperation o) =>
  Maybe (Selection VALID -> ResolverState (Channel e)) ->
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
        runRootDataResolver channelMap query ctx
      selectByOperation Mutation =
        runRootDataResolver channelMap mutation ctx
      selectByOperation Subscription =
        runRootDataResolver channelMap subscription ctx
