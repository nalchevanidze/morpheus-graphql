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

module Data.Morpheus.Server.Deriving.Encode
  ( EncodeCon,
    Encode (..),
    ExploreResolvers (..),
    deriveModel,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
  ( toList,
  )
-- MORPHEUS

import Data.Morpheus.Internal.Utils (Namespace (..))
import Data.Morpheus.Kind
  ( ENUM,
    GQL_KIND,
    INTERFACE,
    OUTPUT,
    ResContext (..),
    SCALAR,
    VContext (..),
  )
import Data.Morpheus.Server.Deriving.Channels (ChannelCon, getChannels)
import Data.Morpheus.Server.Deriving.Decode
  ( DecodeType,
    decodeArguments,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( conNameProxy,
    datatypeNameProxy,
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
  ( FieldName,
    FieldName (..),
    InternalError,
    MUTATION,
    OperationType (..),
    QUERY,
    SUBSCRIPTION,
    TypeName,
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
import Data.Text (pack)
import GHC.Generics

class Encode resolver o e (m :: * -> *) where
  encode :: resolver -> Resolver o e m (ResModel o e m)

instance {-# OVERLAPPABLE #-} (EncodeKind (KIND a) a o e m, LiftOperation o) => Encode a o e m where
  encode resolver = encodeKind (VContext resolver :: VContext (KIND a) a)

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
instance (Eq k, Monad m, LiftOperation o, Encode (MapKind k v (Resolver o e m)) o e m) => Encode (Map k v) o e m where
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
instance
  ( Monad m,
    Encode b o e m,
    LiftOperation o
  ) =>
  Encode (Resolver o e m b) o e m
  where
  encode x = x >>= encode

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a o e (m :: * -> *) where
  encodeKind :: LiftOperation o => VContext kind a -> Resolver o e m (ResModel o e m)

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a o e m where
  encodeKind = pure . ResScalar . serialize . unVContext

-- ENUM
instance (Generic a, ExploreResolvers a o e m, Monad m) => EncodeKind ENUM a o e m where
  encodeKind (VContext value) = liftResolverState $ exploreResolvers value

instance (Monad m, Generic a, ExploreResolvers a o e m) => EncodeKind OUTPUT a o e m where
  encodeKind (VContext value) = liftResolverState $ exploreResolvers value

instance (Monad m, Generic a, ExploreResolvers a o e m) => EncodeKind INTERFACE a o e m where
  encodeKind (VContext value) = liftResolverState $ exploreResolvers value

convertNode ::
  (Monad m, LiftOperation o) =>
  ResNode o e m ->
  ResModel o e m
convertNode ResNode {resDatatypeName, resKind = REP_OBJECT, resFields} =
  mkObject resDatatypeName (map toFieldRes resFields)
convertNode ResNode {resDatatypeName, resKind = REP_UNION, resFields, resTypeName, isResRecord} =
  encodeUnion resFields
  where
    -- ENUM
    encodeUnion [] = ResEnum resDatatypeName resTypeName
    -- Type References --------------------------------------------------------------
    encodeUnion [FieldNode {fieldTypeName, fieldResolver, isFieldObject}]
      | isFieldObject && resTypeName == resDatatypeName <> fieldTypeName =
        ResUnion fieldTypeName fieldResolver
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
          | otherwise = setFieldNames fields

-- Types & Constrains -------------------------------------------------------
type GQL_RES a = (Generic a, GQLType a)

type EncodeCon o e m a = (GQL_RES a, ExploreResolvers a o e m)

--- GENERICS ------------------------------------------------
class ExploreResolvers a (o :: OperationType) e (m :: * -> *) where
  exploreResolvers :: a -> ResolverState (ResModel o e m)

instance (Generic a, GQLType a, Monad m, LiftOperation o, TypeRep (Rep a) o e m) => ExploreResolvers a o e m where
  exploreResolvers =
    pure
      . convertNode
      . stripNamespace (hasNamespace (Proxy @a))
      . typeResolvers (ResContext :: ResContext OUTPUT o e m value)
      . from

----- HELPERS ----------------------------
objectResolvers ::
  forall a o e m.
  ( ExploreResolvers a o e m,
    Monad m,
    LiftOperation o
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

type Con o e m a = ExploreResolvers (a (Resolver o e m)) o e m

deriveModel ::
  forall e m query mut sub.
  ( Con QUERY e m query,
    Con MUTATION e m mut,
    Con SUBSCRIPTION e m sub,
    ChannelCon e m sub,
    Applicative m,
    Monad m,
    GQLType (sub (Resolver SUBSCRIPTION e m))
  ) =>
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

toFieldRes :: FieldNode o e m -> FieldResModel o e m
toFieldRes FieldNode {fieldSelName, fieldResolver} =
  (fieldSelName, fieldResolver)

-- NEW AUTOMATIC DERIVATION SYSTEM
data REP_KIND = REP_UNION | REP_OBJECT

data ResNode o e m = ResNode
  { resDatatypeName :: TypeName,
    resTypeName :: TypeName,
    resKind :: REP_KIND,
    resFields :: [FieldNode o e m],
    isResRecord :: Bool
  }

instance Namespace (ResNode o e m) where
  stripNamespace ns r = r {resFields = map (stripNamespace ns) (resFields r)}

instance Namespace (FieldNode o e m) where
  stripNamespace ns f = f {fieldSelName = stripNamespace ns (fieldSelName f)}

data FieldNode o e m = FieldNode
  { fieldTypeName :: TypeName,
    fieldSelName :: FieldName,
    fieldResolver :: Resolver o e m (ResModel o e m),
    isFieldObject :: Bool
  }

-- setFieldNames ::  Power Int Text -> Power { _1 :: Int, _2 :: Text }
setFieldNames :: [FieldNode o e m] -> [FieldNode o e m]
setFieldNames = zipWith setFieldName ([0 ..] :: [Int])
  where
    setFieldName i field = field {fieldSelName = FieldName $ "_" <> pack (show i)}

class TypeRep f o e (m :: * -> *) where
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

instance (FieldRep f o e m, Constructor c) => TypeRep (M1 C c f) o e m where
  typeResolvers context (M1 src) =
    ResNode
      { resDatatypeName = "",
        resTypeName = conNameProxy (Proxy @c),
        resKind = REP_OBJECT,
        resFields = fieldRep context src,
        isResRecord = isRecordProxy (Proxy @c)
      }

--- FIELDS
class FieldRep f o e (m :: * -> *) where
  fieldRep :: ResContext OUTPUT o e m value -> f a -> [FieldNode o e m]

instance (FieldRep f o e m, FieldRep g o e m) => FieldRep (f :*: g) o e m where
  fieldRep context (a :*: b) = fieldRep context a <> fieldRep context b

instance (Selector s, GQLType a, Encode a o e m) => FieldRep (M1 S s (K1 s2 a)) o e m where
  fieldRep _ (M1 (K1 src)) =
    [ FieldNode
        { fieldSelName = selNameProxy (Proxy @s),
          fieldTypeName = __typeName (Proxy @a),
          fieldResolver = encode src,
          isFieldObject = isObjectKind (Proxy @a)
        }
    ]

instance FieldRep U1 o e m where
  fieldRep _ _ = []
