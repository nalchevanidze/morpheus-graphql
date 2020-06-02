{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Rendering.RenderIntrospection
  ( render,
    createObjectType,
  )
where

import Data.Maybe (isJust)
-- Morpheus

import Data.Morpheus.Internal.Utils
  ( elems,
    failure,
    selectBy,
  )
import qualified Data.Morpheus.Rendering.RenderGQL as GQL (RenderGQL (..))
import Data.Morpheus.Schema.TypeKind (TypeKind (..))
import qualified Data.Morpheus.Types.Internal.AST as AST (TypeKind (..))
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    DataEnumValue (..),
    DataInputUnion,
    DataInputUnion,
    DataTypeWrapper (..),
    DataUnion,
    Description,
    DirectiveDefinition (..),
    DirectiveLocation,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    IN,
    Message,
    OUT,
    QUERY,
    RESOLVED,
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    VALID,
    Value,
    createInputUnionFields,
    fieldVisibility,
    kindOf,
    lookupDeprecated,
    lookupDeprecatedReason,
    msg,
    toGQLWrapper,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Context (..),
    ResModel,
    Resolver,
    mkBoolean,
    mkList,
    mkNull,
    mkObject,
    mkString,
    unsafeInternalContext,
  )
import Data.Semigroup ((<>))
import Data.Text (pack)

type Result e m a = Resolver QUERY e m a

getSchema ::
  Monad m =>
  Resolver QUERY e m Schema
getSchema = schema <$> unsafeInternalContext

selectType ::
  Monad m =>
  TypeName ->
  Resolver QUERY e m (TypeDefinition ANY)
selectType name =
  getSchema
    >>= selectBy (" INTERNAL: INTROSPECTION Type not Found: \"" <> msg name <> "\"") name

class RenderSchema a where
  render ::
    (Monad m) =>
    a ->
    Resolver QUERY e m (ResModel QUERY e m)

instance RenderSchema DirectiveDefinition where
  render
    DirectiveDefinition
      { directiveDefinitionName,
        directiveDefinitionDescription,
        directiveDefinitionLocations,
        directiveDefinitionArgs
      } =
      pure $
        mkObject
          "__Directive"
          [ renderFieldName directiveDefinitionName,
            description directiveDefinitionDescription,
            ("locations", render directiveDefinitionLocations),
            ("args", mkList <$> renderArguments directiveDefinitionArgs)
          ]

instance RenderSchema a => RenderSchema [a] where
  render ls = mkList <$> traverse render ls

instance RenderSchema DirectiveLocation where
  render locations = pure $ mkString (pack $ show locations)

instance RenderSchema (TypeDefinition a) where
  render
    TypeDefinition
      { typeName,
        typeDescription,
        typeContent
      } = __render typeContent
      where
        __render ::
          (Monad m) => TypeContent bool a -> Resolver QUERY e m (ResModel QUERY e m)
        __render DataScalar {} =
          createLeafType SCALAR typeName typeDescription Nothing
        __render (DataEnum enums) =
          createLeafType ENUM typeName typeDescription (Just $ map createEnumValue enums)
        __render (DataInputObject fields) =
          createInputObject typeName typeDescription
            <$> traverse renderInputValue (elems fields)
        __render DataObject {objectImplements, objectFields} =
          createObjectType typeName typeDescription objectImplements objectFields
        __render (DataUnion union) =
          typeFromUnion (typeName, typeDescription, union)
        __render (DataInputUnion members) =
          renderInputUnion (typeName, typeDescription, members)
        __render (DataInterface fields) =
          renderInterface typeName Nothing fields

renderFields :: Monad m => FieldsDefinition cat -> Resolver QUERY e m [ResModel QUERY e m]
renderFields = traverse render . filter fieldVisibility . elems

renderInterface ::
  Monad m => TypeName -> Maybe Description -> FieldsDefinition OUT -> Resolver QUERY e m (ResModel QUERY e m)
renderInterface name desc fields =
  pure $
    mkObject
      "__Type"
      [ renderKind INTERFACE,
        renderName name,
        description desc,
        ("fields", mkList <$> renderFields fields),
        ("possibleTypes", mkList <$> interfacePossibleTypes name)
      ]

interfacePossibleTypes ::
  (Monad m) =>
  TypeName ->
  Resolver QUERY e m [ResModel QUERY e m]
interfacePossibleTypes interfaceName =
  getSchema
    >>= sequence
      . concatMap implements
      . elems
  where
    implements typeDef@TypeDefinition {typeContent = DataObject {objectImplements}, ..}
      | interfaceName `elem` objectImplements = [render typeDef]
    implements _ = []

createEnumValue :: Monad m => DataEnumValue -> ResModel QUERY e m
createEnumValue DataEnumValue {enumName, enumDescription, enumDirectives} =
  mkObject "__Field" $
    [ renderName enumName,
      description enumDescription
    ]
      <> renderDeprecated enumDirectives

renderDeprecated ::
  (Monad m) =>
  Directives VALID ->
  [(FieldName, Resolver QUERY e m (ResModel QUERY e m))]
renderDeprecated dirs =
  [ ("isDeprecated", pure $ mkBoolean (isJust $ lookupDeprecated dirs)),
    ("deprecationReason", opt (pure . mkString) (lookupDeprecated dirs >>= lookupDeprecatedReason))
  ]

description :: Monad m => Maybe Description -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
description desc = ("description", opt (pure . mkString) desc)

renderArguments :: (Monad m) => ArgumentsDefinition -> Result e m [ResModel QUERY e m]
renderArguments ArgumentsDefinition {arguments} = traverse renderInputValue (elems arguments)

instance RenderSchema (FieldDefinition cat) where
  render
    field@FieldDefinition
      { fieldName,
        fieldType = TypeRef {typeConName},
        fieldContent,
        fieldDescription,
        fieldDirectives
      } =
      do
        kind <- lookupKind typeConName
        pure
          $ mkObject "__Field"
          $ [ renderFieldName fieldName,
              description fieldDescription,
              ("args", mkList <$> renderFieldArgs fieldContent),
              ("type", pure (withTypeWrapper field $ createType kind typeConName Nothing $ Just []))
            ]
            <> renderDeprecated fieldDirectives

renderFieldArgs :: (Monad m) => Maybe (FieldContent TRUE cat) -> Resolver QUERY e m [ResModel QUERY e m]
renderFieldArgs (Just (FieldArgs args)) = renderArguments args
renderFieldArgs _ = pure []

lookupKind :: (Monad m) => TypeName -> Result e m TypeKind
lookupKind = fmap (renderTypeKind . kindOf) . selectType

renderTypeKind :: AST.TypeKind -> TypeKind
renderTypeKind AST.KindScalar = SCALAR
renderTypeKind (AST.KindObject _) = OBJECT
renderTypeKind AST.KindUnion = UNION
renderTypeKind AST.KindInputUnion = INPUT_OBJECT
renderTypeKind AST.KindEnum = ENUM
renderTypeKind AST.KindInputObject = INPUT_OBJECT
renderTypeKind AST.KindList = LIST
renderTypeKind AST.KindNonNull = NON_NULL
renderTypeKind AST.KindInterface = INTERFACE

renderInputValue ::
  (Monad m) =>
  FieldDefinition IN ->
  Result e m (ResModel QUERY e m)
renderInputValue input@FieldDefinition {fieldName, fieldDescription, fieldContent, fieldType} =
  createInputValueWith
    fieldName
    fieldType
    fieldDescription
    (fmap defaultInputValue fieldContent)
    <$> createInputObjectType input

createInputObjectType ::
  (Monad m) => FieldDefinition IN -> Result e m (ResModel QUERY e m)
createInputObjectType field@FieldDefinition {fieldType = TypeRef {typeConName}} =
  do
    kind <- lookupKind typeConName
    pure $ withTypeWrapper field $ createType kind typeConName Nothing $ Just []

renderInputUnion ::
  (Monad m) =>
  (TypeName, Maybe Description, DataInputUnion) ->
  Result e m (ResModel QUERY e m)
renderInputUnion (key, meta, fields) =
  createInputObject key meta
    <$> traverse
      createField
      (createInputUnionFields key $ map fst $ filter snd fields)
  where
    createField field@FieldDefinition {fieldType} =
      createInputValueWith (fieldName field) fieldType Nothing Nothing <$> createInputObjectType field

createLeafType ::
  Monad m =>
  TypeKind ->
  TypeName ->
  Maybe Description ->
  Maybe [ResModel QUERY e m] ->
  Result e m (ResModel QUERY e m)
createLeafType kind name desc enums =
  pure $
    mkObject
      "__Type"
      [ renderKind kind,
        renderName name,
        description desc,
        ("enumValues", optList enums)
      ]

typeFromUnion :: Monad m => (TypeName, Maybe Description, DataUnion) -> Result e m (ResModel QUERY e m)
typeFromUnion (name, desc, typeContent) =
  pure $
    mkObject
      "__Type"
      [ renderKind UNION,
        renderName name,
        description desc,
        ("possibleTypes", mkList <$> traverse unionPossibleType typeContent)
      ]

unionPossibleType :: Monad m => TypeName -> Resolver QUERY e m (ResModel QUERY e m)
unionPossibleType name = selectType name >>= render

createObjectType ::
  Monad m => TypeName -> Maybe Description -> [TypeName] -> FieldsDefinition OUT -> Result e m (ResModel QUERY e m)
createObjectType name desc interfaces fields =
  pure $
    mkObject
      "__Type"
      [ renderKind OBJECT,
        renderName name,
        description desc,
        ("fields", mkList <$> renderFields fields),
        ("interfaces", mkList <$> traverse implementedInterface interfaces)
      ]

implementedInterface ::
  (Monad m) =>
  TypeName ->
  Resolver QUERY e m (ResModel QUERY e m)
implementedInterface name =
  selectType name
    >>= __render
  where
    __render typeDef@TypeDefinition {typeContent = DataInterface {}} = render typeDef
    __render _ = failure ("Type " <> msg name <> " must be an Interface" :: Message)

optList :: Monad m => Maybe [ResModel QUERY e m] -> Resolver QUERY e m (ResModel QUERY e m)
optList = pure . maybe mkNull mkList

createInputObject ::
  Monad m => TypeName -> Maybe Description -> [ResModel QUERY e m] -> ResModel QUERY e m
createInputObject name desc fields =
  mkObject
    "__Type"
    [ renderKind INPUT_OBJECT,
      renderName name,
      description desc,
      ("inputFields", pure $ mkList fields)
    ]

createType ::
  Monad m =>
  TypeKind ->
  TypeName ->
  Maybe Description ->
  Maybe [ResModel QUERY e m] ->
  ResModel QUERY e m
createType kind name desc fields =
  mkObject
    "__Type"
    [ renderKind kind,
      renderName name,
      description desc,
      ("fields", pure $ maybe mkNull mkList fields),
      ("enumValues", pure $ mkList [])
    ]

opt :: Monad m => (a -> Resolver QUERY e m (ResModel QUERY e m)) -> Maybe a -> Resolver QUERY e m (ResModel QUERY e m)
opt f (Just x) = f x
opt _ Nothing = pure mkNull

renderName :: Monad m => TypeName -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderName = ("name",) . pure . mkString . readTypeName

renderFieldName :: Monad m => FieldName -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderFieldName (FieldName name) = ("name", pure $ mkString name)

renderKind :: Monad m => TypeKind -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderKind = ("kind",) . pure . mkString . pack . show

withTypeWrapper :: Monad m => FieldDefinition cat -> ResModel QUERY e m -> ResModel QUERY e m
withTypeWrapper FieldDefinition {fieldType = TypeRef {typeWrappers}} typ =
  foldr wrapAs typ (toGQLWrapper typeWrappers)

wrapAs :: Monad m => DataTypeWrapper -> ResModel QUERY e m -> ResModel QUERY e m
wrapAs wrapper contentType =
  mkObject
    "__Type"
    [ renderKind (kind wrapper),
      ("ofType", pure contentType)
    ]
  where
    kind ListType = LIST
    kind NonNullType = NON_NULL

defaultValue ::
  Monad m =>
  TypeRef ->
  Maybe (Value RESOLVED) ->
  (FieldName, Resolver QUERY e m (ResModel QUERY e m))
defaultValue
  TypeRef {typeConName}
  desc =
    ( "defaultValue",
      opt (pure . mkString . GQL.render) desc
    )

createInputValueWith ::
  Monad m =>
  FieldName ->
  TypeRef ->
  Maybe Description ->
  Maybe (Value RESOLVED) ->
  ResModel QUERY e m ->
  ResModel QUERY e m
createInputValueWith name tyRef desc value ivType =
  mkObject
    "__InputValue"
    [ renderFieldName name,
      description desc,
      ("type", pure ivType),
      defaultValue tyRef value
    ]
