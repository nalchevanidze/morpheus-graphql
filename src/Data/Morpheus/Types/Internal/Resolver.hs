{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Types.Internal.Resolver
  ( Pure
  , Resolver
  , MutResolver
  , SubResolver
  , ResolveT
  , SubResolveT
  , Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
  , resolver
  , GQLFail(..)
  , ResponseT
  , failResolveT
  , GADTResolver(..)
  , GraphQLT(..)
  , MapGraphQLT(..)
  , PureOperation(..)
  , liftResolver
  , convertResolver
  , toResponseRes
  ) where

import           Control.Monad                              (join)
import           Control.Monad.Trans.Except                 (ExceptT (..), runExceptT, withExceptT)
import           Data.Text                                  (pack, unpack)
-- MORPHEUS
import           Data.Morpheus.Error.Selection              (resolverError)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..))
import           Data.Morpheus.Types.Internal.Base          (Message, Position)
import           Data.Morpheus.Types.Internal.Data          (Key, MUTATION, OperationKind, QUERY, SUBSCRIPTION)
import           Data.Morpheus.Types.Internal.Stream        (Channel (..), Event (..), ResponseEvent (..),
                                                             ResponseStream, StreamChannel, StreamState (..),
                                                             StreamT (..), SubscribeStream, closeStream, injectEvents,
                                                             initExceptStream, mapS)
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, Validation)
import           Data.Morpheus.Types.Internal.Value         (GQLValue (..), Value)
import           Data.Morpheus.Types.IO                     (renderResponse)

class Monad m =>
      GQLFail (t :: (* -> *) -> * -> *) m
  where
  gqlFail :: Monad m => Message -> t m a
  toSuccess :: Monad m => (Message -> b) -> (a -> b) -> t m a -> t m b

instance Monad m => GQLFail Resolver m where
  gqlFail = ExceptT . pure . Left . unpack
  toSuccess fFail fSuc (ExceptT value) = ExceptT $ pure . mapCases <$> value
    where
      mapCases (Right x) = fSuc x
      mapCases (Left x)  = fFail $ pack $ show x

----------------------------------------------------------------------------------------
type SubResolver = GADTResolver SUBSCRIPTION

type MutResolver = GADTResolver MUTATION

type Resolver = ExceptT String

type ResolveT = ExceptT GQLErrors

------------------------------------------------------------
--- Transformers
type ResponseT m e  = ResolveT (ResponseStream m e)

type SubResolveT = GraphQLT SUBSCRIPTION

data GraphQLT (o::OperationKind) (m :: * -> * ) event value where
    QueryT:: ResolveT m value -> GraphQLT QUERY m  event value
    MutationT :: ResolveT (StreamT m event) value -> GraphQLT MUTATION m event value
    SubscriptionT ::  ResolveT (SubscribeStream m event) (event -> ResolveT m value) -> GraphQLT SUBSCRIPTION m event value
    FailT :: GQLErrors -> GraphQLT o m  event value

data GADTResolver (o::OperationKind) (m :: * -> * ) event value where
    FailedResolver :: String -> GADTResolver o m event value
    QueryResolver:: ExceptT String m value -> GADTResolver QUERY m  event value
    MutationResolver :: [event] -> m value -> GADTResolver MUTATION m event value
    SubscriptionResolver :: [StreamChannel event] -> (event -> GADTResolver QUERY m  event value) -> GADTResolver SUBSCRIPTION m event value

instance Functor m => Functor (GraphQLT o m e) where
    fmap _ (FailT mErrors) = FailT mErrors
    fmap f (QueryT mResolver) = QueryT $ fmap f mResolver
    fmap f (MutationT mResolver) = MutationT $ fmap f mResolver
    fmap f (SubscriptionT mResolver) = SubscriptionT (eventFmap <$> mResolver)
            where
                eventFmap res event = fmap f (res event)

class PureOperation (o::OperationKind) where
    pureGraphQLT :: Monad m => a -> GraphQLT o m event a
    pureGADTResolver :: Monad m => a -> GADTResolver o m event a
    eitherGraphQLT :: Monad m => Validation a -> GraphQLT o m event a

instance PureOperation QUERY where
   pureGraphQLT = QueryT . pure
   pureGADTResolver = QueryResolver . pure
   eitherGraphQLT = QueryT . ExceptT . pure

instance PureOperation MUTATION where
   pureGraphQLT = MutationT . pure
   pureGADTResolver = MutationResolver [] . pure
   eitherGraphQLT = MutationT . ExceptT . pure

instance PureOperation SUBSCRIPTION where
   pureGraphQLT = SubscriptionT . pure . const . pure
   pureGADTResolver = SubscriptionResolver []  . const . pure
   eitherGraphQLT = SubscriptionT . pure . const . ExceptT . pure

instance (PureOperation o, Monad m) => Applicative (GraphQLT o m e) where
    pure = pureGraphQLT
    -------------------------------------
    _ <*> (FailT mErrors) = FailT mErrors
    (FailT mErrors) <*> _ = FailT mErrors
    -------------------------------------
    (QueryT f) <*> (QueryT res) = QueryT (f <*> res)
    -------------------------------------
    (MutationT f) <*> (MutationT res) = MutationT (f <*> res)
    --------------------------------------------------------------
    (SubscriptionT f) <*> (SubscriptionT res) = SubscriptionT $ do
                       f1 <- f
                       res1 <- res
                       pure $ \event -> f1 event <*>  res1 event

unQueryT :: Applicative m => GraphQLT QUERY m e a -> ResolveT m a
unQueryT (QueryT x) = x
unQueryT (FailT x)  = ExceptT  $ pure $  Left x

unMutationT :: Applicative m => GraphQLT MUTATION m e a -> ResolveT (StreamT m e) a
unMutationT (MutationT x) = x
unMutationT (FailT x)     = ExceptT $ StreamT $ pure $ StreamState [] $ Left x

unSubscriptionT :: Applicative m => GraphQLT SUBSCRIPTION m event value -> ResolveT (SubscribeStream m event) (event -> ResolveT m value)
unSubscriptionT (SubscriptionT x) = x
unSubscriptionT (FailT x)         = ExceptT $ StreamT $ pure $ StreamState [] $ Left x

instance (Monad m, PureOperation o)  => Monad (GraphQLT o m e) where
    return = pure
    (QueryT value) >>= nextM = QueryT (value >>= unQueryT . nextM)
    (MutationT value) >>= nextM = MutationT (value >>= unMutationT . nextM)
    -- :TODO implement subscription
    --(M m_e_to_ma) >>= a_to_m1_Meb = M $ wow <$> m_e_to_ma
    --         where
    --           wow e_to_ma e = do
    --             a <- e_to_ma e
    --             unM (a_to_m1_Meb a) >>=  (\x -> x e)
    (SubscriptionT startValue)  >>= nextM = SubscriptionT $ do 
                  let (channels , resTStartValue ) = closeSubStream startValue
                  injectEvents2 channels (genResponse <$> resTStartValue)
                  where
                    genResponse event_to_ma event = do
                        value_a <- event_to_ma event
                        (snd $ closeSubStream $ unSubscriptionT (nextM value_a)) >>= (\x -> (x event))


injectEvents2 :: Functor m => [event] -> ExceptT errors m a -> ExceptT errors (StreamT m event) a
injectEvents2 states = ExceptT . StreamT . fmap (StreamState states) . runExceptT

closeSubStream :: Monad m => ResolveT (SubscribeStream m e) (e -> ResolveT m v) -> ([[Channel e]], ResolveT m (e -> ResolveT m v))
closeSubStream (ExceptT mon) = mon


--closeStream :: Monad m => (StreamT m s) v -> m ([s], v)
--closeStream resolver = toTuple <$> runStreamT resolver

-- (a -> (Key,Selection) -> ResolveT m a) -> (Key,Selection)
convertResolver :: Monad m =>  Position -> Key -> GADTResolver o m e a ->  GraphQLT o m e a
    --FailT $ resolverError selectionPosition fieldName message
convertResolver position fieldName = convert
 where
    convert (QueryResolver res)           = QueryT $ withExceptT (resolverError position fieldName) res
    convert (FailedResolver message)      = FailT $ resolverError position fieldName message
    convert (MutationResolver events res) = MutationT $ ExceptT $ StreamT (StreamState events . Right <$> res)
    --convert (SubscriptionResolver subChannels subResolver) = SubscriptionT $ initExceptStream [map Channel subChannels] ((encode selection . subResolver) :: event -> ResolveT m a)

-- [StreamChannel event] -> (event -> GADTResolver QUERY m  event value) ->

-- ResolveT (SubscribeStream m event) (event -> ResolveT m value)

--  encode resolver selection =  handleResolver resolver
--    where
--      handleResolver (SubscriptionResolver subChannels subResolver) =
--        SubscriptionT $ initExceptStream [map Channel subChannels] ((encodeResolver selection . subResolver) :: event -> ResolveT m Value)
      --handleResolver (FailedResolving  errorMessage) = TODO: handle error

class MapGraphQLT (fromO :: OperationKind) (toO :: OperationKind) where
   mapGraphQLT :: GraphQLT fromO m e a -> GraphQLT toO m e a

instance MapGraphQLT fromO fromO where
    mapGraphQLT = id

instance MapGraphQLT QUERY SUBSCRIPTION where
    --mapGraphQLT = id

liftResolver :: (Monad m, PureOperation o) => (a -> (Key,Selection) -> GraphQLT o m e value) -> (Key, Selection) -> GADTResolver o m e a  -> GraphQLT o m e value
liftResolver encode  selection@(fieldName, Selection {selectionPosition}) res = withRes res >>= (`encode` selection)
   where
    withRes :: Monad m => GADTResolver o m e a ->  GraphQLT o m e a
    withRes  = convertResolver selectionPosition fieldName

toResponseRes :: Monad m =>  GraphQLT o m event Value -> ResponseT m event Value
toResponseRes (FailT errors) = ExceptT $ StreamT $ pure $ StreamState [] $ Left errors
toResponseRes (QueryT resT) =  ExceptT $ StreamT $ StreamState [] <$> runExceptT resT
toResponseRes (MutationT resT) = ExceptT $ mapS Publish (runExceptT resT)
toResponseRes (SubscriptionT resT)  =
      ExceptT $ StreamT $ handleActions <$> closeStream (runExceptT resT)
      where
        handleActions (_, Left gqlError) = StreamState [] (Left gqlError)
        handleActions (channels, Right subResolver) =
          StreamState [Subscribe $ Event (concat channels) handleRes] (Right  gqlNull)
          where
            handleRes event = renderResponse <$> runExceptT (subResolver event)


instance Functor m => Functor (GADTResolver o m e) where
    fmap _ (FailedResolver mErrors) = FailedResolver mErrors
    fmap f (QueryResolver mResolver) = QueryResolver $ fmap f mResolver
    fmap f (MutationResolver events mResolver) = MutationResolver events $ fmap f mResolver
    fmap f (SubscriptionResolver events mResolver) = SubscriptionResolver events (eventFmap mResolver)
            where
                eventFmap res event = fmap f (res event)

instance (PureOperation o ,Monad m) => Applicative (GADTResolver o m e) where
    pure = pureGADTResolver
    -------------------------------------
    _ <*> (FailedResolver mErrors) = FailedResolver mErrors
    (FailedResolver mErrors) <*> _ = FailedResolver mErrors
    -------------------------------------
    (QueryResolver f) <*> (QueryResolver res) = QueryResolver (f <*> res)
    ---------------------------------------------------------------------
    (MutationResolver events1 f) <*> (MutationResolver events2 res) = MutationResolver (events1 <> events2) (f <*> res)
    --------------------------------------------------------------
    (SubscriptionResolver e1 f) <*> (SubscriptionResolver e2 res) = SubscriptionResolver (e1<>e2) $
                       \event -> f event <*>  res event

type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (SubResolver m e) = GADTResolver QUERY m e

-------------------------------------------------------------------
failResolveT :: Monad m => GQLErrors -> ResolveT m a
failResolveT = ExceptT . pure . Left

-------------------------------------------------------------------
-- | Pure Resolver without effect
type Pure = Either String

-- | GraphQL Resolver
resolver :: m (Either String a) -> Resolver m a
resolver = ExceptT

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: GADTResolver QUERY m event (query (GADTResolver QUERY m  event))
  , mutationResolver     :: MutResolver m event (mut (MutResolver m event))
  , subscriptionResolver :: SubResolver m event (sub (SubResolver  m event))
  }
