{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.GQLType
  ( GQLType
      ( KIND,
        implements,
        description,
        getDescriptions,
        typeOptions,
        getDirectives,
        getFieldContents
      ),
    GQLTypeOptions
      ( fieldLabelModifier,
        constructorTagModifier,
        typeNameModifier
      ),
    defaultTypeOptions,
    TypeData (..),
    __isObjectKind,
    __isEmptyType,
    __typeData,
  )
where

-- MORPHEUS

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    SubscriptionField,
  )
import Data.Morpheus.Kind
  ( CUSTOM,
    DerivingKind,
    SCALAR,
    TYPE,
    ToValue,
    WRAPPER,
    isObject,
    toValue,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
    TypeFingerprint (..),
  )
import Data.Morpheus.Server.Types.Types
  ( Pair,
    Undefined (..),
  )
import Data.Morpheus.Types.ID (ID)
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    Description,
    Directives,
    FieldName,
    OUT,
    QUERY,
    TypeCategory (..),
    TypeName (..),
    TypeWrapper (..),
    Value,
    mkBaseType,
    toNullable,
  )
import Data.Morpheus.Utils.Kinded (CategoryValue (..))
import Data.Text
  ( intercalate,
    pack,
    unpack,
  )
import Data.Typeable
  ( TyCon,
    TypeRep,
    splitTyConApp,
    tyConFingerprint,
    tyConName,
    typeRep,
    typeRepTyCon,
  )
import Relude hiding (Undefined, intercalate)

data TypeData = TypeData
  { gqlTypeName :: TypeName,
    gqlWrappers :: TypeWrapper,
    gqlFingerprint :: TypeFingerprint
  }

data GQLTypeOptions = GQLTypeOptions
  { fieldLabelModifier :: String -> String,
    constructorTagModifier :: String -> String,
    -- Construct a new type name depending on whether it is an input,
    -- and being given the original type name
    typeNameModifier :: Bool -> String -> String
  }

defaultTypeOptions :: GQLTypeOptions
defaultTypeOptions =
  GQLTypeOptions
    { fieldLabelModifier = id,
      constructorTagModifier = id,
      -- default is just a pass through for the original type name
      typeNameModifier = const id
    }

__typeData ::
  forall kinded (kind :: TypeCategory) (a :: *).
  (GQLType a, CategoryValue kind) =>
  kinded kind a ->
  TypeData
__typeData proxy = __type proxy (categoryValue (Proxy @kind))

getTypename :: Typeable a => f a -> TypeName
getTypename = TypeName . intercalate "" . getTypeConstructorNames

getTypeConstructorNames :: Typeable a => f a -> [Text]
getTypeConstructorNames = fmap (pack . tyConName . replacePairCon) . getTypeConstructors

getTypeConstructors :: Typeable a => f a -> [TyCon]
getTypeConstructors = ignoreResolver . splitTyConApp . typeRep

deriveTypeData :: Typeable a => f a -> (Bool -> String -> String) -> TypeCategory -> TypeData
deriveTypeData proxy typeNameModifier cat =
  TypeData
    { gqlTypeName = TypeName . pack $ typeNameModifier (cat == IN) originalTypeName,
      gqlWrappers = mkBaseType,
      gqlFingerprint = getFingerprint cat proxy
    }
  where
    originalTypeName = unpack . readTypeName $ getTypename proxy

getFingerprint :: Typeable a => TypeCategory -> f a -> TypeFingerprint
getFingerprint category = TypeableFingerprint category . fmap tyConFingerprint . getTypeConstructors

mkTypeData :: TypeName -> a -> TypeData
mkTypeData name _ =
  TypeData
    { gqlTypeName = name,
      gqlFingerprint = InternalFingerprint name,
      gqlWrappers = mkBaseType
    }

list :: TypeWrapper -> TypeWrapper
list = flip TypeList True

wrapper :: (TypeWrapper -> TypeWrapper) -> TypeData -> TypeData
wrapper f TypeData {..} = TypeData {gqlWrappers = f gqlWrappers, ..}

resolverCon :: TyCon
resolverCon = typeRepTyCon $ typeRep $ Proxy @(Resolver QUERY () Maybe)

-- | replaces typeName (A,B) with Pair_A_B
replacePairCon :: TyCon -> TyCon
replacePairCon x | hsPair == x = gqlPair
  where
    hsPair = typeRepTyCon $ typeRep $ Proxy @(Int, Int)
    gqlPair = typeRepTyCon $ typeRep $ Proxy @(Pair Int Int)
replacePairCon x = x

-- Ignores Resolver name  from typeName
ignoreResolver :: (TyCon, [TypeRep]) -> [TyCon]
ignoreResolver (con, _) | con == resolverCon = []
ignoreResolver (con, args) =
  con : concatMap (ignoreResolver . splitTyConApp) args

__isObjectKind :: forall f a. GQLType a => f a -> Bool
__isObjectKind _ = isObject $ toValue (Proxy @(KIND a))

-- | GraphQL type, every graphQL type should have an instance of 'GHC.Generics.Generic' and 'GQLType'.
--
--  @
--    ... deriving (Generic, GQLType)
--  @
--
-- if you want to add description
--
--  @
--       ... deriving (Generic)
--
--     instance GQLType ... where
--       description = const "your description ..."
--  @
class ToValue (KIND a) => GQLType a where
  type KIND a :: DerivingKind
  type KIND a = TYPE

  implements :: f a -> [SchemaT OUT TypeName]
  implements _ = []

  description :: f a -> Maybe Text
  description _ = Nothing

  getDescriptions :: f a -> Map Text Description
  getDescriptions _ = mempty

  typeOptions :: f a -> GQLTypeOptions -> GQLTypeOptions
  typeOptions _ = id

  getDirectives :: f a -> Map Text (Directives CONST)
  getDirectives _ = mempty

  getFieldContents ::
    f a ->
    Map
      FieldName
      ( Maybe (Value CONST),
        Maybe (ArgumentsDefinition CONST)
      )
  getFieldContents _ = mempty

  __isEmptyType :: f a -> Bool
  __isEmptyType _ = False

  __type :: f a -> TypeCategory -> TypeData
  default __type :: Typeable a => f a -> TypeCategory -> TypeData
  __type proxy = deriveTypeData proxy typeNameModifier
    where
      GQLTypeOptions {typeNameModifier} = typeOptions proxy defaultTypeOptions

instance GQLType Int where
  type KIND Int = SCALAR
  __type _ = mkTypeData "Int"

instance GQLType Double where
  type KIND Double = SCALAR
  __type _ = mkTypeData "Float"

instance GQLType Float where
  type KIND Float = SCALAR
  __type _ = mkTypeData "Float32"

instance GQLType Text where
  type KIND Text = SCALAR
  __type _ = mkTypeData "String"

instance GQLType Bool where
  type KIND Bool = SCALAR
  __type _ = mkTypeData "Boolean"

instance GQLType ID where
  type KIND ID = SCALAR
  __type _ = mkTypeData "ID"

-- WRAPPERS
instance GQLType ()

instance Typeable m => GQLType (Undefined m) where
  type KIND (Undefined m) = WRAPPER
  __isEmptyType _ = True

instance GQLType a => GQLType (Maybe a) where
  type KIND (Maybe a) = WRAPPER
  __type _ = wrapper toNullable . __type (Proxy @a)

instance GQLType a => GQLType [a] where
  type KIND [a] = WRAPPER
  __type _ = wrapper list . __type (Proxy @a)

instance GQLType a => GQLType (Set a) where
  type KIND (Set a) = WRAPPER
  __type _ = __type $ Proxy @[a]

instance GQLType a => GQLType (SubscriptionField a) where
  type KIND (SubscriptionField a) = WRAPPER
  __type _ = __type $ Proxy @a

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (Pair a b)

-- Manual
instance GQLType b => GQLType (a -> b) where
  type KIND (a -> b) = CUSTOM
  __type _ = __type $ Proxy @b

instance (GQLType k, GQLType v, Typeable k, Typeable v) => GQLType (Map k v) where
  type KIND (Map k v) = CUSTOM
  __type _ = __type $ Proxy @[Pair k v]

instance GQLType a => GQLType (Resolver o e m a) where
  type KIND (Resolver o e m a) = CUSTOM
  __type _ = __type $ Proxy @a

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (a, b) where
  type KIND (a, b) = CUSTOM
  __type _ = __type $ Proxy @(Pair a b)
