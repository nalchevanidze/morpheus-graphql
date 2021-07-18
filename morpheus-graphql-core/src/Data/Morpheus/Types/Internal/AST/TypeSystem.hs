{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.TypeSystem
  ( ScalarDefinition (..),
    DataEnum,
    UnionTypeDefinition,
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    DataEnumValue (..),
    TypeDefinitions,
    TypeCategory,
    mkEnumContent,
    mkUnionContent,
    mkType,
    createScalarType,
    initTypeLib,
    kindOf,
    isLeaf,
    lookupWith,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    SchemaDefinition (..),
    buildSchema,
    Typed (Typed),
    untyped,
    typed,
    possibleTypes,
    possibleInterfaceTypes,
    defineSchemaWith,
    isPossibleInterfaceType,
    typeDefinitions,
    lookupDataType,
  )
where

-- MORPHEUS

-- MORPHEUS

import qualified Data.HashMap.Lazy as HM
import Data.Mergeable
  ( IsMap (lookup),
    Merge (..),
    NameCollision (..),
    OrdMap,
  )
import Data.Mergeable.SafeHashMap
  ( SafeHashMap,
    toHashMap,
  )
import Data.Morpheus.Internal.Utils
  ( (<:>),
    Empty (..),
    Failure (..),
    IsMap (..),
    KeyOf (..),
    insert,
    selectOr,
    toPair,
    unsafeFromList,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    intercalate,
    newline,
    renderEntry,
    renderMembers,
    renderObject,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    TRUE,
    Token,
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationError,
    ValidationErrors,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.AST.Fields
  ( DirectiveDefinition (..),
    Directives,
    DirectivesDefinition,
    FieldsDefinition,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( TypeName,
    isNotSystemTypeName,
    unpackName,
  )
import Data.Morpheus.Types.Internal.AST.OperationType
  ( OperationType (..),
    toOperationType,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( CONST,
    Stage,
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.Type
  ( Strictness (..),
    TypeKind (..),
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( ANY,
    FromCategory (..),
    IMPLEMENTABLE,
    IN,
    INPUT_OBJECT,
    LEAF,
    OBJECT,
    OUT,
    ToCategory (..),
    TypeCategory,
    fromAny,
    toAny,
    type (<=!),
    type (<=?),
  )
import Data.Morpheus.Types.Internal.AST.Union
  ( UnionTypeDefinition,
    mkInputUnionFields,
    mkUnionMember,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( Value (..),
  )
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding
  ( empty,
    intercalate,
    show,
  )
import Prelude (Show (..))

type DataEnum s = [DataEnumValue s]

-- used for preserving type information from untyped values
-- e.g
-- unionType :: UnionMember IN VALID -> Typed IN VALID TypeName
-- unionType = typed memberName
typed :: (a c s -> b) -> a c s -> Typed c s b
typed f = Typed . f

untyped :: (a -> b) -> Typed c s a -> b
untyped f = f . _untyped

-- | used for preserving type information from untyped values
-- see function typed
newtype Typed (cat :: TypeCategory) (s :: Stage) a = Typed
  { _untyped :: a
  }

-- scalar
------------------------------------------------------------------
newtype ScalarDefinition = ScalarDefinition
  {validateValue :: Value VALID -> Either Token (Value VALID)}

instance Eq ScalarDefinition where
  _ == _ = False

instance Show ScalarDefinition where
  show _ = "ScalarDefinition"

instance Lift ScalarDefinition where
  lift _ = [|ScalarDefinition pure|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped _ = [||ScalarDefinition pure||]
#endif

-- ENUM VALUE
data DataEnumValue s = DataEnumValue
  { enumDescription :: Maybe Description,
    enumName :: TypeName,
    enumDirectives :: Directives s
  }
  deriving (Show, Lift, Eq)

instance RenderGQL (DataEnumValue s) where
  renderGQL DataEnumValue {enumName} = renderGQL enumName

-- 3.2 Schema : https://graphql.github.io/graphql-spec/June2018/#sec-Schema
---------------------------------------------------------------------------
-- SchemaDefinition :
--    schema Directives[Const](opt) { RootOperationTypeDefinition(list)}
--
-- RootOperationTypeDefinition :
--    OperationType: NamedType

data Schema (s :: Stage) = Schema
  { types :: TypeDefinitions s,
    query :: TypeDefinition OBJECT s,
    mutation :: Maybe (TypeDefinition OBJECT s),
    subscription :: Maybe (TypeDefinition OBJECT s),
    directiveDefinitions :: DirectivesDefinition s
  }
  deriving (Show, Lift)

instance
  ( Monad m,
    Failure ValidationErrors m
  ) =>
  Merge m (Schema s)
  where
  merge s1 s2 =
    Schema
      <$> merge (types s1) (types s2)
      <*> mergeOperation (query s1) (query s2)
      <*> mergeOptional (mutation s1) (mutation s2)
      <*> mergeOptional (subscription s1) (subscription s2)
      <*> directiveDefinitions s1 <:> directiveDefinitions s2

mergeOptional ::
  (Monad m, Failure ValidationErrors m) =>
  Maybe (TypeDefinition OBJECT s) ->
  Maybe (TypeDefinition OBJECT s) ->
  m (Maybe (TypeDefinition OBJECT s))
mergeOptional Nothing y = pure y
mergeOptional (Just x) Nothing = pure (Just x)
mergeOptional (Just x) (Just y) = Just <$> mergeOperation x y

mergeOperation ::
  (Monad m, Failure ValidationErrors m) =>
  TypeDefinition OBJECT s ->
  TypeDefinition OBJECT s ->
  m (TypeDefinition OBJECT s)
mergeOperation
  TypeDefinition {typeContent = DataObject i1 fields1}
  TypeDefinition {typeContent = DataObject i2 fields2, ..} =
    do
      fields <- merge fields1 fields2
      pure $ TypeDefinition {typeContent = DataObject (i1 <> i2) fields, ..}

data SchemaDefinition = SchemaDefinition
  { schemaDirectives :: Directives CONST,
    unSchemaDefinition :: OrdMap OperationType RootOperationTypeDefinition
  }
  deriving (Show)

instance RenderGQL SchemaDefinition where
  renderGQL = renderSchemaDefinition . toList . unSchemaDefinition

renderSchemaDefinition :: [RootOperationTypeDefinition] -> Rendering
renderSchemaDefinition entries = "schema" <> renderObject entries <> newline

instance NameCollision SchemaDefinition where
  nameCollision _ = "There can Be only One SchemaDefinition."

instance KeyOf TypeName SchemaDefinition where
  keyOf _ = "schema"

data RawTypeDefinition
  = RawSchemaDefinition SchemaDefinition
  | RawTypeDefinition (TypeDefinition ANY CONST)
  | RawDirectiveDefinition (DirectiveDefinition CONST)
  deriving (Show)

data RootOperationTypeDefinition = RootOperationTypeDefinition
  { rootOperationType :: OperationType,
    rootOperationTypeDefinitionName :: TypeName
  }
  deriving (Show, Eq)

instance NameCollision RootOperationTypeDefinition where
  nameCollision RootOperationTypeDefinition {rootOperationType} =
    "There can Be only One TypeDefinition for schema." <> msgValidation rootOperationType

instance KeyOf OperationType RootOperationTypeDefinition where
  keyOf = rootOperationType

instance RenderGQL RootOperationTypeDefinition where
  renderGQL
    RootOperationTypeDefinition
      { rootOperationType,
        rootOperationTypeDefinitionName
      } = renderEntry rootOperationType rootOperationTypeDefinitionName

type TypeDefinitions s = SafeHashMap TypeName (TypeDefinition ANY s)

typeDefinitions :: Schema s -> HashMap TypeName (TypeDefinition ANY s)
typeDefinitions schema@Schema {..} = toHashMap types <> HM.fromList operations
  where
    operations = map toPair $ rootTypeDefinitions schema

rootTypeDefinitions :: Schema s -> [TypeDefinition ANY s]
rootTypeDefinitions Schema {..} = map toAny $ catMaybes [Just query, mutation, subscription]

mkSchema :: (Monad m, Failure ValidationErrors m) => [TypeDefinition ANY s] -> m (Schema s)
mkSchema types =
  traverse3
    (popByKey types)
    ( RootOperationTypeDefinition Query "Query",
      RootOperationTypeDefinition Mutation "Mutation",
      RootOperationTypeDefinition Subscription "Subscription"
    )
    >>= defineSchemaWith types

defineSchemaWith ::
  ( Monad f,
    Failure ValidationErrors f
  ) =>
  [TypeDefinition cat s] ->
  ( Maybe (TypeDefinition OBJECT s),
    Maybe (TypeDefinition OBJECT s),
    Maybe (TypeDefinition OBJECT s)
  ) ->
  f (Schema s)
defineSchemaWith oTypes (Just query, mutation, subscription) = do
  let types = excludeTypes [Just query, mutation, subscription] oTypes
  let schema = (initTypeLib query) {mutation, subscription}
  foldlM (flip defineType) schema types
defineSchemaWith _ (Nothing, _, _) = failure ["Query root type must be provided." :: ValidationError]

excludeTypes :: [Maybe (TypeDefinition c1 s)] -> [TypeDefinition c2 s] -> [TypeDefinition c2 s]
excludeTypes exclusionTypes = filter ((`notElem` blacklist) . typeName)
  where
    blacklist :: [TypeName]
    blacklist = fmap typeName (catMaybes exclusionTypes)

withDirectives ::
  (Monad m, Failure [ValidationError] m) =>
  DirectivesDefinition s ->
  Schema s ->
  m (Schema s)
withDirectives dirs Schema {..} = do
  dirs' <- directiveDefinitions <:> dirs
  pure $
    Schema
      { directiveDefinitions = dirs',
        ..
      }

buildSchema ::
  (Monad m, Failure ValidationErrors m) =>
  ( Maybe SchemaDefinition,
    [TypeDefinition ANY s],
    DirectivesDefinition s
  ) ->
  m (Schema s)
buildSchema (Nothing, types, dirs) = mkSchema types >>= withDirectives dirs
buildSchema (Just schemaDef, types, dirs) =
  traverse3 selectOp (Query, Mutation, Subscription)
    >>= defineSchemaWith types
    >>= withDirectives dirs
  where
    selectOp op = selectOperation schemaDef op types

traverse3 :: Applicative t => (a -> t b) -> (a, a, a) -> t (b, b, b)
traverse3 f (a1, a2, a3) = (,,) <$> f a1 <*> f a2 <*> f a3

typeReference ::
  (Monad m, Failure ValidationErrors m) =>
  [TypeDefinition ANY s] ->
  RootOperationTypeDefinition ->
  m (Maybe (TypeDefinition OBJECT s))
typeReference types rootOperation =
  popByKey types rootOperation
    >>= maybe
      (failure ["Unknown type " <> msgValidation (rootOperationTypeDefinitionName rootOperation) <> "."])
      (pure . Just)

selectOperation ::
  ( Monad f,
    Failure ValidationErrors f
  ) =>
  SchemaDefinition ->
  OperationType ->
  [TypeDefinition ANY s] ->
  f (Maybe (TypeDefinition OBJECT s))
selectOperation SchemaDefinition {unSchemaDefinition} operationType lib =
  selectOr (pure Nothing) (typeReference lib) operationType unSchemaDefinition

initTypeLib :: TypeDefinition OBJECT s -> Schema s
initTypeLib query =
  Schema
    { types = empty,
      query = query,
      mutation = Nothing,
      subscription = Nothing,
      directiveDefinitions = empty
    }

isType :: TypeName -> TypeDefinition OBJECT s -> Maybe (TypeDefinition ANY s)
isType name x
  | name == typeName x = Just (toAny x)
  | otherwise = Nothing

lookupDataType :: TypeName -> Schema s -> Maybe (TypeDefinition ANY s)
lookupDataType name Schema {types, query, mutation, subscription} =
  isType name query
    <|> (mutation >>= isType name)
    <|> (subscription >>= isType name)
    <|> lookup name types

-- 3.4 Types : https://graphql.github.io/graphql-spec/June2018/#sec-Types
-------------------------------------------------------------------------
-- TypeDefinition :
--   ScalarTypeDefinition
--   ObjectTypeDefinition
--   InterfaceTypeDefinition
--   UnionTypeDefinition
--   EnumTypeDefinition
--   InputObjectTypeDefinition

data TypeDefinition (a :: TypeCategory) (s :: Stage) = TypeDefinition
  { typeDescription :: Maybe Description,
    typeName :: TypeName,
    typeDirectives :: Directives s,
    typeContent :: TypeContent TRUE a s
  }
  deriving (Show, Lift, Eq)

instance Ord (TypeDefinition k s) where
  compare a b =
    compare
      (indexOf $ typeContent a)
      (indexOf $ typeContent b)

instance KeyOf TypeName (TypeDefinition a s) where
  keyOf = typeName

instance Strictness (TypeDefinition k s) where
  isResolverType = isResolverType . typeContent

instance NameCollision (TypeDefinition cat s) where
  nameCollision x = "There can Be only One TypeDefinition Named " <> msgValidation (typeName x) <> "."

instance
  ToCategory (TypeContent TRUE) cat cat' =>
  ToCategory TypeDefinition cat cat'
  where
  toCategory TypeDefinition {typeContent, ..} =
    TypeDefinition
      { typeContent = toCategory typeContent,
        ..
      }

possibleTypes :: TypeDefinition a s -> Schema s' -> [TypeName]
possibleTypes
  TypeDefinition
    { typeName,
      typeContent = DataObject {objectImplements}
    }
  _ = typeName : objectImplements
possibleTypes TypeDefinition {typeName = name, typeContent = DataInterface {}} schema =
  name : fmap typeName (possibleInterfaceTypes name schema)
possibleTypes TypeDefinition {typeName} _ = [typeName]

possibleInterfaceTypes ::
  TypeName ->
  Schema s ->
  [TypeDefinition ANY s]
possibleInterfaceTypes name schema =
  mapMaybe
    (isPossibleInterfaceType name)
    (toList $ typeDefinitions schema)

isPossibleInterfaceType ::
  TypeName ->
  TypeDefinition c s ->
  Maybe (TypeDefinition c s)
isPossibleInterfaceType name typeDef@TypeDefinition {typeName, typeContent = DataObject {objectImplements}}
  | name `elem` (typeName : objectImplements) = Just typeDef
isPossibleInterfaceType _ _ = Nothing

instance
  (FromCategory (TypeContent TRUE) cat cat') =>
  FromCategory TypeDefinition cat cat'
  where
  fromCategory TypeDefinition {typeContent, ..} = bla <$> fromCategory typeContent
    where
      bla x = TypeDefinition {typeContent = x, ..}

type CondTypeContent r a s = TypeContent (r <=? a) a s

data
  TypeContent
    (b :: Bool)
    (a :: TypeCategory)
    (s :: Stage)
  where
  DataScalar ::
    { dataScalar :: ScalarDefinition
    } ->
    CondTypeContent LEAF a s
  DataEnum ::
    { enumMembers :: DataEnum s
    } ->
    CondTypeContent LEAF a s
  DataInputObject ::
    { inputObjectFields :: FieldsDefinition IN s
    } ->
    CondTypeContent INPUT_OBJECT a s
  DataInputUnion ::
    { inputUnionMembers :: UnionTypeDefinition IN s
    } ->
    CondTypeContent IN a s
  DataObject ::
    { objectImplements :: [TypeName],
      objectFields :: FieldsDefinition OUT s
    } ->
    CondTypeContent OBJECT a s
  DataUnion ::
    { unionMembers :: UnionTypeDefinition OUT s
    } ->
    CondTypeContent OUT a s
  DataInterface ::
    { interfaceFields :: FieldsDefinition OUT s
    } ->
    CondTypeContent IMPLEMENTABLE a s

deriving instance Show (TypeContent a b s)

deriving instance Eq (TypeContent a b s)

deriving instance Lift (TypeContent a b s)

indexOf :: TypeContent b a s -> Int
indexOf DataScalar {} = 0
indexOf DataEnum {} = 1
indexOf DataInputObject {} = 2
indexOf DataInputUnion {} = 3
indexOf DataInterface {} = 4
indexOf DataObject {} = 5
indexOf DataUnion {} = 6

instance Strictness (TypeContent TRUE k s) where
  isResolverType DataObject {} = True
  isResolverType DataUnion {} = True
  isResolverType DataInterface {} = True
  isResolverType _ = False

instance ToCategory (TypeContent TRUE) a ANY where
  toCategory DataScalar {..} = DataScalar {..}
  toCategory DataEnum {..} = DataEnum {..}
  toCategory DataInputObject {..} = DataInputObject {..}
  toCategory DataInputUnion {..} = DataInputUnion {..}
  toCategory DataObject {..} = DataObject {..}
  toCategory DataUnion {..} = DataUnion {..}
  toCategory DataInterface {..} = DataInterface {..}

instance ToCategory (TypeContent TRUE) OBJECT IMPLEMENTABLE where
  toCategory DataObject {..} = DataObject {..}

instance ToCategory (TypeContent TRUE) INPUT_OBJECT IN where
  toCategory DataInputObject {..} = DataInputObject {..}

instance FromCategory (TypeContent TRUE) ANY IN where
  fromCategory DataScalar {..} = Just DataScalar {..}
  fromCategory DataEnum {..} = Just DataEnum {..}
  fromCategory DataInputObject {..} = Just DataInputObject {..}
  fromCategory DataInputUnion {..} = Just DataInputUnion {..}
  fromCategory _ = Nothing

instance FromCategory (TypeContent TRUE) ANY OUT where
  fromCategory DataScalar {..} = Just DataScalar {..}
  fromCategory DataEnum {..} = Just DataEnum {..}
  fromCategory DataObject {..} = Just DataObject {..}
  fromCategory DataUnion {..} = Just DataUnion {..}
  fromCategory DataInterface {..} = Just DataInterface {..}
  fromCategory _ = Nothing

instance FromCategory (TypeContent TRUE) ANY OBJECT where
  fromCategory DataObject {..} = Just DataObject {..}
  fromCategory _ = Nothing

instance FromCategory (TypeContent TRUE) ANY IMPLEMENTABLE where
  fromCategory DataObject {..} = Just DataObject {..}
  fromCategory DataInterface {..} = Just DataInterface {..}
  fromCategory _ = Nothing

mkType :: TypeName -> TypeContent TRUE a s -> TypeDefinition a s
mkType typeName typeContent =
  TypeDefinition
    { typeName,
      typeDescription = Nothing,
      typeDirectives = empty,
      typeContent
    }

createScalarType :: (LEAF <=! a) => TypeName -> TypeDefinition a s
createScalarType typeName = mkType typeName $ DataScalar (ScalarDefinition pure)

mkEnumContent :: (LEAF <=! a) => [TypeName] -> TypeContent TRUE a s
mkEnumContent typeData = DataEnum (fmap mkEnumValue typeData)

mkUnionContent :: [TypeName] -> TypeContent TRUE OUT s
mkUnionContent typeData = DataUnion $ unsafeFromList $ map (toPair . mkUnionMember) typeData

mkEnumValue :: TypeName -> DataEnumValue s
mkEnumValue enumName =
  DataEnumValue
    { enumName,
      enumDescription = Nothing,
      enumDirectives = empty
    }

isLeaf :: TypeContent TRUE a s -> Bool
isLeaf DataScalar {} = True
isLeaf DataEnum {} = True
isLeaf _ = False

kindOf :: TypeDefinition a s -> TypeKind
kindOf TypeDefinition {typeName, typeContent} = __kind typeContent
  where
    __kind DataScalar {} = KindScalar
    __kind DataEnum {} = KindEnum
    __kind DataInputObject {} = KindInputObject
    __kind DataObject {} = KindObject (toOperationType typeName)
    __kind DataUnion {} = KindUnion
    __kind DataInputUnion {} = KindInputUnion
    __kind DataInterface {} = KindInterface

defineType ::
  ( Monad m,
    Failure ValidationErrors m
  ) =>
  TypeDefinition k s ->
  Schema s ->
  m (Schema s)
defineType datatype lib = updateTypes <$> insert (toAny datatype) (types lib)
  where
    updateTypes types = lib {types}

lookupWith :: Eq k => (a -> k) -> k -> [a] -> Maybe a
lookupWith f key = find ((== key) . f)

popByKey ::
  (Applicative m, Failure ValidationErrors m) =>
  [TypeDefinition ANY s] ->
  RootOperationTypeDefinition ->
  m (Maybe (TypeDefinition OBJECT s))
popByKey types (RootOperationTypeDefinition opType name) = case lookupWith typeName name types of
  Just dt@TypeDefinition {typeContent = DataObject {}} ->
    pure (fromAny dt)
  Just {} ->
    failure
      [ msgValidation (show opType)
          <> " root type must be Object type if provided, it cannot be "
          <> msgValidation name
      ]
  _ -> pure Nothing

--
-- OTHER
--------------------------------------------------------------------------------------------------

hasDefaultOperationName :: RootOperationTypeDefinition -> Bool
hasDefaultOperationName
  RootOperationTypeDefinition
    { rootOperationType,
      rootOperationTypeDefinitionName = name
    } = show rootOperationType == T.unpack (unpackName name)

instance RenderGQL (Schema s) where
  renderGQL schema@Schema {..} =
    intercalate newline (fmap renderGQL visibleTypes <> schemaDefinition)
    where
      schemaDefinition
        | all hasDefaultOperationName entries = []
        | otherwise = [renderSchemaDefinition entries]
      entries =
        catMaybes
          [ RootOperationTypeDefinition Query . typeName <$> Just query,
            RootOperationTypeDefinition Mutation . typeName <$> mutation,
            RootOperationTypeDefinition Subscription . typeName <$> subscription
          ]
      visibleTypes =
        filter
          (isNotSystemTypeName . typeName)
          (sort $ toList types)
          <> rootTypeDefinitions schema

instance RenderGQL (TypeDefinition a s) where
  renderGQL TypeDefinition {typeName, typeContent} = __render typeContent <> newline
    where
      __render DataInterface {interfaceFields} = "interface " <> renderGQL typeName <> renderGQL interfaceFields
      __render DataScalar {} = "scalar " <> renderGQL typeName
      __render (DataEnum tags) = "enum " <> renderGQL typeName <> renderObject tags
      __render (DataUnion members) =
        "union "
          <> renderGQL typeName
          <> " = "
          <> renderMembers members
      __render (DataInputObject fields) = "input " <> renderGQL typeName <> renderGQL fields
      __render (DataInputUnion members) = "input " <> renderGQL typeName <> renderGQL fields
        where
          fields = mkInputUnionFields members
      __render DataObject {objectFields} = "type " <> renderGQL typeName <> renderGQL objectFields
