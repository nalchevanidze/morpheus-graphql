{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Encode
  ( deriveModel,
    EncodeConstraints,
  )
where

-- MORPHEUS

import Control.Applicative (Applicative (..))
import Control.Monad (Monad ((>>=)))
import Data.Functor (fmap)
import Data.Map (Map)
import qualified Data.Map as M
  ( toList,
  )
import Data.Maybe
  ( Maybe (..),
    maybe,
  )
import Data.Morpheus.Internal.Utils (Namespace (..))
import Data.Morpheus.Kind
  ( ENUM,
    GQL_KIND,
    INTERFACE,
    OUTPUT,
    SCALAR,
  )
import Data.Morpheus.Server.Deriving.Channels
  ( ChannelsConstraint,
    getChannels,
  )
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeType,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( FieldRep (..),
    conNameProxy,
    datatypeNameProxy,
    enumerate,
    isRecordProxy,
    selNameProxy,
  )
import Data.Morpheus.Server.Types.GQLType (GQLType (..))
import Data.Morpheus.Server.Types.Types
  ( MapKind,
    Pair (..),
    mapKindFromList,
  )
import Data.Morpheus.Types
  ( RootResolver (..),
  )
import Data.Morpheus.Types.GQLScalar (GQLScalar (..))
import Data.Morpheus.Types.Internal.AST
  ( InternalError,
    MUTATION,
    OperationType,
    QUERY,
    SUBSCRIPTION,
    TypeName,
    TypeRef (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( FieldResModel,
    LiftOperation,
    ResModel (..),
    Resolver,
    ResolverState,
    RootResModel (..),
    SubscriptionField (..),
    failure,
    getArguments,
    liftResolverState,
    mkObject,
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as S
  ( toList,
  )
import Data.Traversable (traverse)
import GHC.Generics
  ( (:*:) (..),
    (:+:) (..),
    C,
    Constructor,
    D,
    Datatype,
    Generic (..),
    K1 (..),
    M1 (..),
    S,
    Selector,
    U1 (..),
  )
import Prelude
  ( ($),
    (&&),
    (.),
    Bool,
    Eq (..),
    otherwise,
  )

data ResContext (kind :: GQL_KIND) (operation :: OperationType) event (m :: * -> *) value = ResContext

newtype ContextValue (kind :: GQL_KIND) a = ContextValue
  { unContextValue :: a
  }

class Encode resolver o e (m :: * -> *) where
  encode :: resolver -> Resolver o e m (ResModel o e m)

instance {-# OVERLAPPABLE #-} (EncodeKind (KIND a) a o e m, LiftOperation o) => Encode a o e m where
  encode resolver = encodeKind (ContextValue resolver :: ContextValue (KIND a) a)

-- MAYBE
instance (Monad m, LiftOperation o, Encode a o e m) => Encode (Maybe a) o e m where
  encode = maybe (pure ResNull) encode

-- LIST []
instance (Monad m, Encode a o e m, LiftOperation o) => Encode [a] o e m where
  encode = fmap ResList . traverse encode

--  Tuple  (a,b)
instance Encode (Pair k v) o e m => Encode (k, v) o e m where
  encode (key, value) = encode (Pair key value)

--  Set
instance Encode [a] o e m => Encode (Set a) o e m where
  encode = encode . S.toList

--  Map
instance (Monad m, LiftOperation o, Encode (MapKind k v (Resolver o e m)) o e m) => Encode (Map k v) o e m where
  encode value =
    encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver o e m))

-- SUBSCRIPTION
instance (Monad m, LiftOperation o, Encode a o e m) => Encode (SubscriptionField a) o e m where
  encode (SubscriptionField _ res) = encode res

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance
  ( DecodeType a,
    Generic a,
    Monad m,
    LiftOperation o,
    Encode b o e m
  ) =>
  Encode (a -> b) o e m
  where
  encode f =
    getArguments
      >>= liftResolverState . decodeArguments
      >>= encode . f

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (Monad m, Encode b o e m, LiftOperation o) => Encode (Resolver o e m b) o e m where
  encode x = x >>= encode

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a o e (m :: * -> *) where
  encodeKind :: LiftOperation o => ContextValue kind a -> Resolver o e m (ResModel o e m)

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a o e m where
  encodeKind = pure . ResScalar . serialize . unContextValue

type EncodeConstraint o e m a = (Monad m, Generic a, GQLType a, TypeRep (Rep a) o e m)

-- ENUM
instance EncodeConstraint o e m a => EncodeKind ENUM a o e m where
  encodeKind = liftResolverState . exploreResolvers . unContextValue

instance EncodeConstraint o e m a => EncodeKind OUTPUT a o e m where
  encodeKind = liftResolverState . exploreResolvers . unContextValue

instance EncodeConstraint o e m a => EncodeKind INTERFACE a o e m where
  encodeKind = liftResolverState . exploreResolvers . unContextValue

convertNode ::
  (Monad m, LiftOperation o) =>
  Maybe TypeName ->
  ResNode o e m ->
  ResModel o e m
convertNode _ ResNode {resDatatypeName, resKind = REP_OBJECT, resFields} =
  mkObject resDatatypeName (fmap toFieldRes resFields)
convertNode namespace ResNode {resDatatypeName, resKind = REP_UNION, resFields, resTypeName, isResRecord} =
  encodeUnion resFields
  where
    -- ENUM
    encodeUnion [] = ResEnum resDatatypeName (stripNamespace namespace resTypeName)
    -- Type References --------------------------------------------------------------
    encodeUnion [FieldRep {fieldTypeRef = TypeRef {typeConName}, fieldValue, fieldIsObject}]
      | fieldIsObject && resTypeName == resDatatypeName <> typeConName =
        ResUnion typeConName fieldValue
    -- Inline Union Types ----------------------------------------------------------------------------
    encodeUnion fields =
      ResUnion
        resTypeName
        $ pure
        $ mkObject
          resTypeName
          (fmap toFieldRes resolvers)
      where
        resolvers
          | isResRecord = fields
          | otherwise = enumerate fields

-- Types & Constrains -------------------------------------------------------

exploreResolvers ::
  forall o e m a.
  ( EncodeConstraint o e m a,
    LiftOperation o
  ) =>
  a ->
  ResolverState (ResModel o e m)
exploreResolvers =
  pure
    . convertNode (getNamespace (Proxy @a))
    . stripNamespace (getNamespace (Proxy @a))
    . typeResolvers (ResContext :: ResContext OUTPUT o e m value)
    . from

----- HELPERS ----------------------------
objectResolvers ::
  forall a o e m.
  ( Monad m,
    LiftOperation o,
    GQLType a,
    Generic a,
    TypeRep (Rep a) o e m
  ) =>
  a ->
  ResolverState (ResModel o e m)
objectResolvers value =
  exploreResolvers value
    >>= constraintObject
  where
    constraintObject obj@ResObject {} =
      pure obj
    constraintObject _ =
      failure ("resolver must be an object" :: InternalError)

type EncodeObjectConstraint (o :: OperationType) e (m :: * -> *) a =
  TypeConstraint o e m (a (Resolver o e m))

type TypeConstraint (o :: OperationType) e m a =
  ( GQLType a,
    Generic a,
    TypeRep (Rep a) o e m
  )

type EncodeConstraints e m query mut sub =
  ( ChannelsConstraint e m sub,
    EncodeObjectConstraint QUERY e m query,
    EncodeObjectConstraint MUTATION e m mut,
    EncodeObjectConstraint SUBSCRIPTION e m sub
  )

deriveModel ::
  forall e m query mut sub.
  (Monad m, EncodeConstraints e m query mut sub) =>
  RootResolver m e query mut sub ->
  RootResModel e m
deriveModel
  RootResolver
    { queryResolver,
      mutationResolver,
      subscriptionResolver
    } =
    RootResModel
      { query = objectResolvers queryResolver,
        mutation = objectResolvers mutationResolver,
        subscription = objectResolvers subscriptionResolver,
        channelMap
      }
    where
      channelMap
        | isEmptyType (Proxy :: Proxy (sub (Resolver SUBSCRIPTION e m))) = Nothing
        | otherwise = Just (getChannels subscriptionResolver)

toFieldRes :: FieldRep (Resolver o e m (ResModel o e m)) -> FieldResModel o e m
toFieldRes FieldRep {fieldSelector, fieldValue} = (fieldSelector, fieldValue)

-- NEW AUTOMATIC DERIVATION SYSTEM
data REP_KIND = REP_UNION | REP_OBJECT

data ResNode o e m = ResNode
  { resDatatypeName :: TypeName,
    resTypeName :: TypeName,
    resKind :: REP_KIND,
    resFields :: [FieldRep (Resolver o e m (ResModel o e m))],
    isResRecord :: Bool
  }

instance Namespace (ResNode o e m) where
  stripNamespace ns r = r {resFields = fmap (stripNamespace ns) (resFields r)}

class TypeRep f (o :: OperationType) e (m :: * -> *) where
  typeResolvers :: ResContext OUTPUT o e m value -> f a -> ResNode o e m

instance (Datatype d, TypeRep f o e m) => TypeRep (M1 D d f) o e m where
  typeResolvers context (M1 src) =
    (typeResolvers context src)
      { resDatatypeName = datatypeNameProxy (Proxy @d)
      }

--- UNION OR OBJECT
instance (TypeRep a o e m, TypeRep b o e m) => TypeRep (a :+: b) o e m where
  typeResolvers context (L1 x) =
    (typeResolvers context x) {resKind = REP_UNION}
  typeResolvers context (R1 x) =
    (typeResolvers context x) {resKind = REP_UNION}

instance (ConsRep f (Resolver o e m (ResModel o e m)), Constructor c) => TypeRep (M1 C c f) o e m where
  typeResolvers _ (M1 src) =
    ResNode
      { resDatatypeName = "",
        resTypeName = conNameProxy (Proxy @c),
        resKind = REP_OBJECT,
        resFields = fieldRep src,
        isResRecord = isRecordProxy (Proxy @c)
      }

--- FIELDS
class ConsRep f v where
  fieldRep :: f a -> [FieldRep v]

instance (ConsRep f v, ConsRep g v) => ConsRep (f :*: g) v where
  fieldRep (a :*: b) = fieldRep a <> fieldRep b

instance
  (Selector s, GQLType a, Encode a o e m) =>
  ConsRep
    (M1 S s (K1 s2 a))
    (Resolver o e m (ResModel o e m))
  where
  fieldRep (M1 (K1 src)) =
    [ FieldRep
        { fieldSelector = selNameProxy (Proxy @s),
          fieldTypeRef =
            TypeRef
              { typeConName = __typeName (Proxy @a),
                typeWrappers = __wrappers (Proxy @a),
                typeArgs = Nothing
              },
          fieldIsObject = isObjectKind (Proxy @a),
          fieldValue = encode src
        }
    ]

instance ConsRep U1 v where
  fieldRep _ = []
