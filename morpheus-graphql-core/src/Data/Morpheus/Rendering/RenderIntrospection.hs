{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Rendering.RenderIntrospection
  ( render,
    createObjectType,
  )
where

-- Morpheus

import Control.Applicative (pure)
import Control.Monad (Monad (..), sequence)
import Data.Foldable (foldr)
import Data.Functor ((<$>), fmap)
import Data.List (elem, filter)
import Data.Maybe (Maybe (..), isJust, maybe)
import Data.Morpheus.Internal.Utils
  ( Failure,
    elems,
    failure,
    fromElems,
    selectBy,
    selectOr,
  )
import qualified Data.Morpheus.Rendering.RenderGQL as GQL (RenderGQL (..))
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    DataEnumValue (..),
    DataTypeWrapper (..),
    Description,
    DirectiveDefinition (..),
    DirectiveLocation,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    GQLErrors,
    IN,
    Message,
    OUT,
    Object,
    ObjectEntry (..),
    QUERY,
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    UnionMember (..),
    VALID,
    Value (..),
    fieldVisibility,
    kindOf,
    lookupDeprecated,
    lookupDeprecatedReason,
    mkInputUnionFields,
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
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (.),
    Bool,
    concatMap,
    show,
  )

class
  ( Monad m,
    Failure Message m,
    Failure GQLErrors m
  ) =>
  WithSchema m
  where
  getSchema :: m (Schema VALID)

instance Monad m => WithSchema (Resolver QUERY e m) where
  getSchema = schema <$> unsafeInternalContext

selectType ::
  WithSchema m =>
  TypeName ->
  m (TypeDefinition ANY VALID)
selectType name =
  getSchema
    >>= selectBy (" INTERNAL: INTROSPECTION Type not Found: \"" <> msg name <> "\"") name

class RenderIntrospection a where
  render ::
    (Monad m) =>
    a ->
    Resolver QUERY e m (ResModel QUERY e m)

instance RenderIntrospection TypeName where
  render = pure . mkString . readTypeName

instance RenderIntrospection FieldName where
  render = pure . mkString . readName

instance RenderIntrospection Description where
  render = pure . mkString

instance RenderIntrospection a => RenderIntrospection [a] where
  render ls = mkList <$> traverse render ls

instance RenderIntrospection a => RenderIntrospection (Maybe a) where
  render (Just value) = render value
  render Nothing = pure mkNull

instance RenderIntrospection Bool where
  render = pure . mkBoolean

instance RenderIntrospection TypeKind where
  render KindScalar = pure $ mkString "SCALAR"
  render KindObject {} = pure $ mkString "OBJECT"
  render KindUnion = pure $ mkString "UNION"
  render KindInputUnion = pure $ mkString "INPUT_OBJECT"
  render KindEnum = pure $ mkString "ENUM"
  render KindInputObject = pure $ mkString "INPUT_OBJECT"
  render KindList = pure $ mkString "LIST"
  render KindNonNull = pure $ mkString "NON_NULL"
  render KindInterface = pure $ mkString "INTERFACE"

instance RenderIntrospection (DirectiveDefinition VALID) where
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
          [ renderName directiveDefinitionName,
            description directiveDefinitionDescription,
            ("locations", render directiveDefinitionLocations),
            ("args", render directiveDefinitionArgs)
          ]

instance RenderIntrospection DirectiveLocation where
  render locations = pure $ mkString (pack $ show locations)

instance RenderIntrospection (TypeDefinition cat VALID) where
  render
    TypeDefinition
      { typeName,
        typeDescription,
        typeContent
      } = pure $ renderContent typeContent
      where
        __type ::
          Monad m =>
          TypeKind ->
          [(FieldName, Resolver QUERY e m (ResModel QUERY e m))] ->
          ResModel QUERY e m
        __type kind = mkType kind typeName typeDescription
        renderContent ::
          Monad m =>
          TypeContent bool a VALID ->
          ResModel QUERY e m
        renderContent DataScalar {} = __type KindScalar []
        renderContent (DataEnum enums) = __type KindEnum [("enumValues", render enums)]
        renderContent (DataInputObject inputFiels) =
          __type
            KindInputObject
            [("inputFields", render inputFiels)]
        renderContent DataObject {objectImplements, objectFields} =
          createObjectType typeName typeDescription objectImplements objectFields
        renderContent (DataUnion union) =
          __type
            KindUnion
            [("possibleTypes", render union)]
        renderContent (DataInputUnion members) =
          __type
            KindInputObject
            [ ( "inputFields",
                render
                  ( mkInputUnionFields typeName $
                      filter visibility members ::
                      FieldsDefinition IN VALID
                  )
              )
            ]
        renderContent (DataInterface fields) =
          __type
            KindInterface
            [ ("fields", render fields),
              ("possibleTypes", interfacePossibleTypes typeName)
            ]

instance RenderIntrospection (UnionMember OUT s) where
  render UnionMember {memberName} = selectType memberName >>= render

instance
  RenderIntrospection (FieldDefinition cat s) =>
  RenderIntrospection (FieldsDefinition cat s)
  where
  render = render . filter fieldVisibility . elems

instance RenderIntrospection (Value VALID) where
  render Null = pure mkNull
  render x = pure $ mkString $ GQL.render x

instance
  RenderIntrospection
    (FieldDefinition OUT VALID)
  where
  render FieldDefinition {..} =
    pure
      $ mkObject "__Field"
      $ [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          ("args", maybe (pure $ mkList []) render fieldContent)
        ]
        <> renderDeprecated fieldDirectives

instance RenderIntrospection (FieldContent TRUE OUT VALID) where
  render (FieldArgs args) = render args

instance RenderIntrospection (ArgumentsDefinition VALID) where
  render ArgumentsDefinition {arguments} = mkList <$> traverse render (elems arguments)

instance RenderIntrospection (FieldDefinition IN VALID) where
  render FieldDefinition {..} =
    pure $
      mkObject
        "__InputValue"
        [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          defaultValue fieldType (fmap defaultInputValue fieldContent)
        ]

instance RenderIntrospection (DataEnumValue VALID) where
  render DataEnumValue {enumName, enumDescription, enumDirectives} =
    pure $ mkObject "__Field" $
      [ renderName enumName,
        description enumDescription
      ]
        <> renderDeprecated enumDirectives

instance RenderIntrospection TypeRef where
  render TypeRef {typeConName, typeWrappers} = do
    kind <- kindOf <$> selectType typeConName
    let currentType = mkType kind typeConName Nothing []
    pure $ foldr wrap currentType (toGQLWrapper typeWrappers)
    where
      wrap :: Monad m => DataTypeWrapper -> ResModel QUERY e m -> ResModel QUERY e m
      wrap wrapper contentType =
        mkObject
          "__Type"
          [ renderKind (wrapperKind wrapper),
            ("ofType", pure contentType)
          ]
      wrapperKind ListType = KindList
      wrapperKind NonNullType = KindNonNull

interfacePossibleTypes ::
  (Monad m) =>
  TypeName ->
  Resolver QUERY e m (ResModel QUERY e m)
interfacePossibleTypes interfaceName =
  mkList
    <$> ( getSchema
            >>= sequence
              . concatMap implements
              . elems
        )
  where
    implements typeDef@TypeDefinition {typeContent = DataObject {objectImplements}}
      | interfaceName `elem` objectImplements = [render typeDef]
    implements _ = []

renderDeprecated ::
  (Monad m) =>
  Directives s ->
  [(FieldName, Resolver QUERY e m (ResModel QUERY e m))]
renderDeprecated dirs =
  [ ("isDeprecated", render (isJust $ lookupDeprecated dirs)),
    ("deprecationReason", render (lookupDeprecated dirs >>= lookupDeprecatedReason))
  ]

description :: Monad m => Maybe Description -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
description desc = ("description", render desc)

mkType ::
  (Monad m, RenderIntrospection name) =>
  TypeKind ->
  name ->
  Maybe Description ->
  [(FieldName, Resolver QUERY e m (ResModel QUERY e m))] ->
  ResModel QUERY e m
mkType kind name desc etc =
  mkObject
    "__Type"
    ( [ renderKind kind,
        renderName name,
        description desc
      ]
        <> etc
    )

createObjectType ::
  Monad m =>
  TypeName ->
  Maybe Description ->
  [TypeName] ->
  FieldsDefinition OUT VALID ->
  ResModel QUERY e m
createObjectType name desc interfaces fields =
  mkType (KindObject Nothing) name desc [("fields", render fields), ("interfaces", mkList <$> traverse implementedInterface interfaces)]

implementedInterface ::
  (Monad m) =>
  TypeName ->
  Resolver QUERY e m (ResModel QUERY e m)
implementedInterface name =
  selectType name
    >>= renderContent
  where
    renderContent typeDef@TypeDefinition {typeContent = DataInterface {}} = render typeDef
    renderContent _ = failure ("Type " <> msg name <> " must be an Interface" :: Message)

renderName ::
  ( RenderIntrospection name,
    Monad m
  ) =>
  name ->
  (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderName = ("name",) . render

renderKind :: Monad m => TypeKind -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
renderKind = ("kind",) . render

type' :: Monad m => TypeRef -> (FieldName, Resolver QUERY e m (ResModel QUERY e m))
type' ref = ("type", render ref)

defaultValue ::
  Monad m =>
  TypeRef ->
  Maybe (Value VALID) ->
  ( FieldName,
    Resolver QUERY e m (ResModel QUERY e m)
  )
defaultValue
  typeRef
  value =
    ( "defaultValue",
      fulfill typeRef value
        >>= render . Just
    )

fulfill ::
  WithSchema m =>
  TypeRef ->
  Maybe (Value VALID) ->
  m (Value VALID)
fulfill TypeRef {typeConName} (Just (Object fields)) =
  selectType typeConName
    >>= \case
      TypeDefinition
        { typeContent =
            DataInputObject {inputObjectFields}
        } ->
          Object
            <$> ( traverse
                    (handleField fields)
                    (elems inputObjectFields)
                    >>= fromElems
                )
      _ -> failure (msg typeConName <> "is not must be Object")
fulfill typeRef (Just (List values)) =
  List <$> traverse (fulfill typeRef . Just) values
fulfill _ (Just v) = pure v
fulfill _ Nothing = pure Null

handleField ::
  WithSchema m =>
  Object VALID ->
  FieldDefinition IN VALID ->
  m (ObjectEntry VALID)
handleField
  fields
  FieldDefinition
    { fieldName,
      fieldType,
      fieldContent
    } =
    ObjectEntry fieldName
      <$> fulfill
        fieldType
        ( selectOr
            (fmap defaultInputValue fieldContent)
            (Just . entryValue)
            fieldName
            fields
        )
