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
  , SubRootRes
  , Event(..)
  , GQLRootResolver(..)
  , UnSubResolver
  , resolver
  , GQLFail(..)
  , ResponseT
  , failResolveT
  , GADTResolver(..)
  , GraphQLT(..)
  , extractMutResolver
  , PackT(..)
  ) where

import           Control.Monad.Trans.Except              (ExceptT (..))
import           Data.Text                               (pack, unpack)

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Base       (Message)
import           Data.Morpheus.Types.Internal.Data       (OperationKind (..))
import           Data.Morpheus.Types.Internal.Stream     (Event (..), PublishStream, ResponseStream, StreamChannel,
                                                          StreamState (..), StreamT (..), SubscribeStream)
import           Data.Morpheus.Types.Internal.Validation (GQLErrors, Validation)

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
type SubResolver = GADTResolver 'Subscription

type MutResolver = GADTResolver 'Mutation

-- type QueResolver = GADTResolver 'Query

-- TODO: Replace With: newtype MutResolver
-- newtype MutResolver m e a = MutResolver {  unMutResolveT :: Resolver (PublishStream m e) a }

type Resolver = ExceptT String

type ResolveT = ExceptT GQLErrors

------------------------------------------------------------
type SubRootRes m e sub = Resolver (SubscribeStream m e) sub

--- Transformers
type ResponseT m e  = ResolveT (ResponseStream m e)

type SubResolveT = GraphQLT 'Subscription


class PackT (o::OperationKind) m event where
    packT ::  Validation a -> GraphQLT o m event a

instance PackT 'Query m event where
instance PackT 'Subscription m event where
instance PackT 'Mutation m event where

class FromResT (o::OperationKind) event where
    fromResT :: ResolveT m a -> GraphQLT o m event a

instance FromResT 'Query event where
instance FromResT 'Subscription  event where
instance FromResT 'Mutation  event where



--    packT =


-- TODO: use it
data GraphQLT (o::OperationKind) (m :: * -> * ) event value where
    QueryT:: ExceptT GQLErrors m value -> GraphQLT 'Query m  event value
    MutationT :: ResolveT (StreamT m e) value -> GraphQLT 'Mutation m event value
    SubscriptionT ::  ResolveT (SubscribeStream m e) (e -> ResolveT m a) -> GraphQLT 'Subscription m event value
    FailT :: GQLErrors -> GraphQLT o m  event value
    
instance Functor m => Functor (GraphQLT o m e) where

instance Applicative m => Applicative (GraphQLT o m e) where

instance Monad m => Monad (GraphQLT o m e) where


data GADTResolver (o::OperationKind) (m :: * -> * ) event value where
    QueryResolver:: ExceptT String m value -> GADTResolver 'Query m  event value
    MutationResolver :: [event] -> m value -> GADTResolver 'Mutation m event value
    SubscriptionResolver :: [StreamChannel event] -> (event -> Resolver m value) -> GADTResolver 'Subscription m event value
    --FailedResolving :: String -> GADTResolver o m event value

type family UnSubResolver (a :: * -> *) :: (* -> *)

type instance UnSubResolver (SubResolver m e) = Resolver m

-------------------------------------------------------------------

failResolveT :: Monad m => GQLErrors -> ResolveT m a
failResolveT = ExceptT . pure . Left


-------------------------------------------------------------------
-- | Pure Resolver without effect
type Pure = Either String

-- | GraphQL Resolver
resolver :: m (Either String a) -> Resolver m a
resolver = ExceptT

extractMutResolver :: Monad m => MutResolver m e a -> Resolver (PublishStream m e) a
extractMutResolver (MutationResolver channels res) = (ExceptT . StreamT . fmap (StreamState channels . Right) )  res

-- | GraphQL Resolver for mutation or subscription resolver , adds effect to normal resolver
-- mutResolver :: Monad m => [e] -> (StreamT m e) (Either String a) -> MutResolver m e a
-- mutResolver channels = ExceptT . StreamT . fmap effectPlus . runStreamT
--  where
--    effectPlus state = state {streamEvents = channels ++ streamEvents state}

-- | GraphQL Root resolver, also the interpreter generates a GQL schema from it.
--
--  'queryResolver' is required, 'mutationResolver' and 'subscriptionResolver' are optional,
--  if your schema does not supports __mutation__ or __subscription__ , you acn use __()__ for it.
data GQLRootResolver (m :: * -> *) event (query :: (* -> *) -> * ) (mut :: (* -> *) -> * )  (sub :: (* -> *) -> * )  = GQLRootResolver
  { queryResolver        :: Resolver m (query (GADTResolver 'Query m  event))
  , mutationResolver     :: Resolver (PublishStream m event) (mut (MutResolver m event))
  , subscriptionResolver :: SubRootRes m event (sub (SubResolver  m event))
  }
