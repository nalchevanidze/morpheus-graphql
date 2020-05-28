{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Introspect
  ( TypeUpdater,
    Introspect (..),
    DeriveTypeContent (..),
    IntroCon,
    updateLib,
    buildType,
    introspectObjectFields,
    deriveCustomInputObjectType,
    TypeScope (..),
  )
where

import Data.List (partition)
import Data.Map (Map)
-- MORPHEUS

import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( empty,
    singleton,
  )
import Data.Morpheus.Kind
  ( Context (..),
    ENUM,
    GQL_KIND,
    INPUT,
    INTERFACE,
    OUTPUT,
    SCALAR,
  )
import Data.Morpheus.Server.Deriving.Utils
  ( EnumRep (..),
    conNameProxy,
    isRecordProxy,
    selNameProxy,
  )
import Data.Morpheus.Server.Types.GQLType (GQLType (..))
import Data.Morpheus.Server.Types.Types
  ( MapKind,
    Pair,
  )
import Data.Morpheus.Types.Directive (FieldDirective)
import Data.Morpheus.Types.GQLScalar (GQLScalar (..))
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    DataFingerprint (..),
    DataUnion,
    FALSE,
    FieldDefinition (..),
    FieldName,
    FieldName (..),
    FieldsDefinition,
    IN,
    Message,
    Meta (..),
    OUT,
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    TypeUpdater,
    createAlias,
    createEnumValue,
    defineType,
    fieldsToArguments,
    msg,
    toAny,
    toAny,
    toListField,
    toNullableField,
    unsafeFromFields,
    updateSchema,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Failure (..),
    Resolver,
    resolveUpdates,
  )
import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text
  ( pack,
  )
import GHC.Generics

type IntroCon a = (GQLType a, DeriveTypeContent (CUSTOM a) a)

-- |  Generates internal GraphQL Schema for query validation and introspection rendering
class Introspect a where
  isObject :: proxy a -> Bool
  default isObject :: GQLType a => proxy a -> Bool
  isObject _ = isObjectKind (Proxy @a)
  field :: proxy a -> FieldName -> FieldDefinition cat
  introspect :: proxy a -> TypeUpdater

  -----------------------------------------------
  default field ::
    GQLType a =>
    proxy a ->
    FieldName ->
    FieldDefinition cat
  field _ = buildField (Proxy @a) NoArguments

instance {-# OVERLAPPABLE #-} (GQLType a, IntrospectKind (KIND a) a) => Introspect a where
  introspect _ = introspectKind (Context :: Context (KIND a) a)

-- Maybe
instance Introspect a => Introspect (Maybe a) where
  isObject _ = False
  field _ = toNullableField . field (Proxy @a)
  introspect _ = introspect (Proxy @a)

instance Introspect a => Introspect (FieldDirective d a) where
  isObject _ = isObject (Proxy @a)
  field _ = field (Proxy @a)
  introspect _ = introspect (Proxy @a)

-- List
instance Introspect a => Introspect [a] where
  isObject _ = False
  field _ = toListField . field (Proxy @a)
  introspect _ = introspect (Proxy @a)

-- Tuple
instance Introspect (Pair k v) => Introspect (k, v) where
  isObject _ = True
  field _ = field (Proxy @(Pair k v))
  introspect _ = introspect (Proxy @(Pair k v))

-- Set
instance Introspect [a] => Introspect (Set a) where
  isObject _ = False
  field _ = field (Proxy @[a])
  introspect _ = introspect (Proxy @[a])

-- Map
instance Introspect (MapKind k v Maybe) => Introspect (Map k v) where
  isObject _ = True
  field _ = field (Proxy @(MapKind k v Maybe))
  introspect _ = introspect (Proxy @(MapKind k v Maybe))

-- Resolver : a -> Resolver b
instance (GQLType b, DeriveTypeContent 'False a, Introspect b) => Introspect (a -> m b) where
  isObject _ = False
  field _ name = fieldObj {fieldArgs}
    where
      fieldObj = field (Proxy @b) name
      fieldArgs =
        fieldsToArguments $ mockFieldsDefinition $ fst $
          introspectObjectFields
            (Proxy :: Proxy 'False)
            (__typeName (Proxy @b), InputType, Proxy @a)
  introspect _ typeLib =
    resolveUpdates
      typeLib
      (introspect (Proxy @b) : inputs)
    where
      name = "Arguments for " <> __typeName (Proxy @b)
      inputs :: [TypeUpdater]
      inputs =
        snd $ introspectObjectFields (Proxy :: Proxy 'False) (name, InputType, Proxy @a)

--  GQL Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (GQLType b, Introspect b) => Introspect (Resolver fo e m b) where
  isObject _ = False
  field _ = field (Proxy @b)
  introspect _ = introspect (Proxy @b)

-- | Introspect With specific Kind: 'kind': object, scalar, enum ...
class IntrospectKind (kind :: GQL_KIND) a where
  introspectKind :: Context kind a -> TypeUpdater -- Generates internal GraphQL Schema

-- SCALAR
instance (GQLType a, GQLScalar a) => IntrospectKind SCALAR a where
  introspectKind _ = updateLib scalarType [] (Proxy @a)
    where
      scalarType = buildType $ DataScalar $ scalarValidator (Proxy @a)

-- ENUM
instance (GQL_TYPE a, EnumRep (Rep a)) => IntrospectKind ENUM a where
  introspectKind _ = updateLib enumType [] (Proxy @a)
    where
      enumType =
        buildType $ DataEnum $ map (createEnumValue . TypeName) $ enumTags (Proxy @(Rep a))

instance (GQL_TYPE a, DeriveTypeContent (CUSTOM a) a) => IntrospectKind INPUT a where
  introspectKind _ = derivingData (Proxy @a) InputType

instance (GQL_TYPE a, DeriveTypeContent (CUSTOM a) a) => IntrospectKind OUTPUT a where
  introspectKind _ = derivingData (Proxy @a) OutputType

instance (GQL_TYPE a, DeriveTypeContent (CUSTOM a) a) => IntrospectKind INTERFACE a where
  introspectKind _ = updateLib (buildType (DataInterface (mockFieldsDefinition fields) :: TypeContent TRUE OUT)) types (Proxy @a)
    where
      (fields, types) =
        introspectObjectFields
          (Proxy @(CUSTOM a))
          (baseName, OutputType, Proxy @a)
      baseName = __typeName (Proxy @a)

derivingData ::
  forall a cat.
  (GQLType a, DeriveTypeContent (CUSTOM a) a) =>
  Proxy a ->
  TypeScope cat ->
  TypeUpdater
derivingData _ scope = updateLib (buildType datatypeContent) updates (Proxy @a)
  where
    (datatypeContent, updates) =
      deriveTypeContent
        (Proxy @(CUSTOM a))
        (Proxy @a, unzip $ implements (Proxy @a), scope, baseName, baseFingerprint)
    baseName = __typeName (Proxy @a)
    baseFingerprint = __typeFingerprint (Proxy @a)

type GQL_TYPE a = (Generic a, GQLType a)

deriveCustomInputObjectType ::
  DeriveTypeContent TRUE a =>
  (TypeName, proxy a) ->
  TypeUpdater
deriveCustomInputObjectType (name, proxy) =
  flip
    resolveUpdates
    (deriveCustomObjectType (name, InputType, proxy))

deriveCustomObjectType ::
  DeriveTypeContent TRUE a =>
  (TypeName, TypeScope cat, proxy a) ->
  [TypeUpdater]
deriveCustomObjectType = snd . introspectObjectFields (Proxy :: Proxy TRUE)

introspectObjectFields ::
  DeriveTypeContent custom a =>
  proxy1 (custom :: Bool) ->
  (TypeName, TypeScope cat, proxy2 a) ->
  (FieldsDefinition cat, [TypeUpdater])
introspectObjectFields p1 (name, scope, proxy) =
  withObject name (deriveTypeContent p1 (proxy, ([], []), scope, "", DataFingerprint "" []))

withObject :: TypeName -> (TypeContent TRUE (cat :: TypeCategory), [TypeUpdater]) -> (FieldsDefinition cat2, [TypeUpdater])
withObject _ (DataObject {objectFields}, ts) = (mockFieldsDefinition objectFields, ts)
withObject _ (DataInputObject {inputObjectFields}, ts) = (mockFieldsDefinition inputObjectFields, ts)
withObject name _ = (empty, [introspectFailure (msg name <> " should have only one nonempty constructor")])

introspectFailure :: Message -> TypeUpdater
introspectFailure = const . failure . globalErrorMessage . ("invalid schema: " <>)

-- Object Fields
class DeriveTypeContent (custom :: Bool) a where
  deriveTypeContent :: proxy1 custom -> (proxy2 a, ([TypeName], [TypeUpdater]), TypeScope cat, TypeName, DataFingerprint) -> (TypeContent TRUE ANY, [TypeUpdater])

instance (TypeRep (Rep a), Generic a) => DeriveTypeContent FALSE a where
  deriveTypeContent _ (_, interfaces, scope, baseName, baseFingerprint) =
    fa $ builder $ typeRep $ Proxy @(Rep a)
    where
      fa (x, y) = (toAny x, y)
      builder [ConsRep {consFields}] = buildObject interfaces scope consFields
      builder cons = genericUnion scope cons
        where
          genericUnion InputType = buildInputUnion (baseName, baseFingerprint)
          genericUnion OutputType = buildUnionType (baseName, baseFingerprint) DataUnion (DataObject [])

buildField :: GQLType a => Proxy a -> ArgumentsDefinition -> FieldName -> FieldDefinition cat
buildField proxy fieldArgs fieldName =
  FieldDefinition
    { fieldType = createAlias $ __typeName proxy,
      fieldMeta = Nothing,
      ..
    }

buildType :: GQLType a => TypeContent TRUE cat -> Proxy a -> TypeDefinition cat
buildType typeContent proxy =
  TypeDefinition
    { typeName = __typeName proxy,
      typeFingerprint = __typeFingerprint proxy,
      typeMeta =
        Just
          Meta
            { metaDescription = description proxy,
              metaDirectives = []
            },
      typeContent
    }

updateLib ::
  GQLType a =>
  (Proxy a -> TypeDefinition cat) ->
  [TypeUpdater] ->
  Proxy a ->
  TypeUpdater
updateLib f stack proxy = updateSchema (__typeName proxy) (__typeFingerprint proxy) stack f proxy

-- NEW AUTOMATIC DERIVATION SYSTEM

data ConsRep = ConsRep
  { consName :: TypeName,
    consIsRecord :: Bool,
    consFields :: [FieldRep]
  }

data FieldRep = FieldRep
  { fieldTypeName :: TypeName,
    fieldData :: FieldDefinition ANY,
    fieldTypeUpdater :: TypeUpdater,
    fieldIsObject :: Bool
  }

data ResRep = ResRep
  { enumCons :: [TypeName],
    unionRef :: [TypeName],
    unionRecordRep :: [ConsRep]
  }

isEmpty :: ConsRep -> Bool
isEmpty ConsRep {consFields = []} = True
isEmpty _ = False

isUnionRef :: TypeName -> ConsRep -> Bool
isUnionRef baseName ConsRep {consName, consFields = [FieldRep {fieldIsObject = True, fieldTypeName}]} =
  consName == baseName <> fieldTypeName
isUnionRef _ _ = False

setFieldNames :: ConsRep -> ConsRep
setFieldNames cons@ConsRep {consFields} =
  cons
    { consFields = zipWith setFieldName ([0 ..] :: [Int]) consFields
    }
  where
    setFieldName i fieldR@FieldRep {fieldData = fieldD} = fieldR {fieldData = fieldD {fieldName}}
      where
        fieldName = FieldName ("_" <> pack (show i))

analyseRep :: TypeName -> [ConsRep] -> ResRep
analyseRep baseName cons =
  ResRep
    { enumCons = map consName enumRep,
      unionRef = map fieldTypeName $ concatMap consFields unionRefRep,
      unionRecordRep = unionRecordRep <> map setFieldNames anyonimousUnionRep
    }
  where
    (enumRep, left1) = partition isEmpty cons
    (unionRefRep, left2) = partition (isUnionRef baseName) left1
    (unionRecordRep, anyonimousUnionRep) = partition consIsRecord left2

buildInputUnion ::
  (TypeName, DataFingerprint) -> [ConsRep] -> (TypeContent TRUE IN, [TypeUpdater])
buildInputUnion (baseName, baseFingerprint) cons =
  datatype
    (analyseRep baseName cons)
  where
    datatype :: ResRep -> (TypeContent TRUE IN, [TypeUpdater])
    datatype ResRep {unionRef = [], unionRecordRep = [], enumCons} =
      (DataEnum (map createEnumValue enumCons), types)
    datatype ResRep {unionRef, unionRecordRep, enumCons} =
      (DataInputUnion typeMembers, types <> unionTypes)
      where
        typeMembers :: [(TypeName, Bool)]
        typeMembers =
          map (,True) (unionRef <> unionMembers) <> map (,False) enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapInputObject baseFingerprint unionRecordRep
    types = map fieldTypeUpdater $ concatMap consFields cons
    wrapInputObject :: (FieldsDefinition IN -> TypeContent TRUE IN)
    wrapInputObject = DataInputObject

buildUnionType ::
  (TypeName, DataFingerprint) ->
  (DataUnion -> TypeContent TRUE cat) ->
  (FieldsDefinition cat -> TypeContent TRUE cat) ->
  [ConsRep] ->
  (TypeContent TRUE cat, [TypeUpdater])
buildUnionType (baseName, baseFingerprint) wrapUnion wrapObject cons =
  datatype
    (analyseRep baseName cons)
  where
    --datatype :: ResRep -> (TypeContent TRUE cat, [TypeUpdater])
    datatype ResRep {unionRef = [], unionRecordRep = [], enumCons} =
      (DataEnum (map createEnumValue enumCons), types)
    datatype ResRep {unionRef, unionRecordRep, enumCons} =
      (wrapUnion typeMembers, types <> enumTypes <> unionTypes)
      where
        typeMembers = unionRef <> enumMembers <> unionMembers
        (enumMembers, enumTypes) =
          buildUnionEnum wrapObject baseName baseFingerprint enumCons
        (unionMembers, unionTypes) =
          buildUnions wrapObject baseFingerprint unionRecordRep
    types = map fieldTypeUpdater $ concatMap consFields cons

buildObject :: ([TypeName], [TypeUpdater]) -> TypeScope cat -> [FieldRep] -> (TypeContent TRUE cat, [TypeUpdater])
buildObject (interfaces, interfaceTypes) scope consFields =
  ( wrapWith scope (mockFieldsDefinition fields),
    types <> interfaceTypes
  )
  where
    (fields, types) = buildDataObject consFields
    --- wrap with
    wrapWith :: TypeScope cat -> FieldsDefinition cat -> TypeContent TRUE cat
    wrapWith InputType = DataInputObject
    wrapWith OutputType = DataObject interfaces

buildDataObject :: [FieldRep] -> (FieldsDefinition ANY, [TypeUpdater])
buildDataObject consFields = (fields, types)
  where
    fields = unsafeFromFields $ map fieldData consFields
    types = map fieldTypeUpdater consFields

buildUnions ::
  (FieldsDefinition cat -> TypeContent TRUE cat) ->
  DataFingerprint ->
  [ConsRep] ->
  ([TypeName], [TypeUpdater])
buildUnions wrapObject baseFingerprint cons = (members, map buildURecType cons)
  where
    buildURecType consRep =
      pure
        . defineType
          (buildUnionRecord wrapObject baseFingerprint consRep)
    members = map consName cons

mockFieldsDefinition :: FieldsDefinition a -> FieldsDefinition b
mockFieldsDefinition = fmap mockFieldDefinition

mockFieldDefinition :: FieldDefinition a -> FieldDefinition b
mockFieldDefinition FieldDefinition {..} = FieldDefinition {..}

buildUnionRecord ::
  (FieldsDefinition cat -> TypeContent TRUE cat) -> DataFingerprint -> ConsRep -> TypeDefinition cat
buildUnionRecord wrapObject typeFingerprint ConsRep {consName, consFields} =
  TypeDefinition
    { typeName = consName,
      typeFingerprint,
      typeMeta = Nothing,
      typeContent =
        wrapObject
          $ mockFieldsDefinition
          $ unsafeFromFields
          $ map fieldData consFields
    }

buildUnionEnum ::
  (FieldsDefinition cat -> TypeContent TRUE cat) ->
  TypeName ->
  DataFingerprint ->
  [TypeName] ->
  ([TypeName], [TypeUpdater])
buildUnionEnum wrapObject baseName baseFingerprint enums = (members, updates)
  where
    members
      | null enums = []
      | otherwise = [enumTypeWrapperName]
    enumTypeName = baseName <> "Enum"
    enumTypeWrapperName = enumTypeName <> "Object"
    -------------------------
    updates :: [TypeUpdater]
    updates
      | null enums =
        []
      | otherwise =
        [ buildEnumObject
            wrapObject
            enumTypeWrapperName
            baseFingerprint
            enumTypeName,
          buildEnum enumTypeName baseFingerprint enums
        ]

buildEnum :: TypeName -> DataFingerprint -> [TypeName] -> TypeUpdater
buildEnum typeName typeFingerprint tags =
  pure
    . defineType
      TypeDefinition
        { typeMeta = Nothing,
          typeContent = DataEnum $ map createEnumValue tags,
          ..
        }

buildEnumObject ::
  (FieldsDefinition cat -> TypeContent TRUE cat) ->
  TypeName ->
  DataFingerprint ->
  TypeName ->
  TypeUpdater
buildEnumObject wrapObject typeName typeFingerprint enumTypeName =
  pure
    . defineType
      TypeDefinition
        { typeName,
          typeFingerprint,
          typeMeta = Nothing,
          typeContent =
            wrapObject $
              singleton
                FieldDefinition
                  { fieldName = "enum",
                    fieldArgs = NoArguments,
                    fieldType = createAlias enumTypeName,
                    fieldMeta = Nothing
                  }
        }

data TypeScope (cat :: TypeCategory) where
  InputType :: TypeScope IN
  OutputType :: TypeScope OUT

deriving instance Show (TypeScope cat)

deriving instance Eq (TypeScope cat)

deriving instance Ord (TypeScope cat)

--  GENERIC UNION
class TypeRep f where
  typeRep :: Proxy f -> [ConsRep]

instance TypeRep f => TypeRep (M1 D d f) where
  typeRep _ = typeRep (Proxy @f)

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
instance (TypeRep a, TypeRep b) => TypeRep (a :+: b) where
  typeRep _ = typeRep (Proxy @a) <> typeRep (Proxy @b)

instance (ConRep f, Constructor c) => TypeRep (M1 C c f) where
  typeRep _ =
    [ ConsRep
        { consName = conNameProxy (Proxy @c),
          consFields = conRep (Proxy @f),
          consIsRecord = isRecordProxy (Proxy @c)
        }
    ]

class ConRep f where
  conRep :: Proxy f -> [FieldRep]

-- | recursion for Object types, both of them : 'UNION' and 'INPUT_UNION'
instance (ConRep a, ConRep b) => ConRep (a :*: b) where
  conRep _ = conRep (Proxy @a) <> conRep (Proxy @b)

instance (Selector s, Introspect a) => ConRep (M1 S s (Rec0 a)) where
  conRep _ =
    [ FieldRep
        { fieldTypeName = typeConName $ fieldType fieldData,
          fieldData = fieldData,
          fieldTypeUpdater = introspect (Proxy @a),
          fieldIsObject = isObject (Proxy @a)
        }
    ]
    where
      name = selNameProxy (Proxy @s)
      fieldData = field (Proxy @a) name

instance ConRep U1 where
  conRep _ = []
