{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.RenderIntrospection
  ( render,
    createObjectType,
    WithSchema,
  )
where

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
    ResolverContext (..),
    ResolverValue,
    mkBoolean,
    mkList,
    mkNull,
    mkObject,
    mkString,
    unsafeInternalContext,
  )
import qualified Data.Morpheus.Core as GQL
import Data.Morpheus.Internal.Utils
  ( Failure,
    elems,
    failure,
    fromLBS,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition (..),
    ArgumentsDefinition,
    DataEnumValue (..),
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
    QUERY,
    Schema,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (BaseType, TypeList),
    UnionMember (..),
    VALID,
    Value (..),
    fieldVisibility,
    kindOf,
    lookupDeprecated,
    lookupDeprecatedReason,
    mkInputUnionFields,
    msg,
    possibleInterfaceTypes,
  )
import Data.Text (pack)
import Relude

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
  render :: (Monad m, WithSchema m) => a -> m (ResolverValue m)

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
  render = pure . mkString . fromLBS . GQL.render

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
          ( Monad m,
            WithSchema m
          ) =>
          TypeKind ->
          [(FieldName, m (ResolverValue m))] ->
          ResolverValue m
        __type kind = mkType kind typeName typeDescription
        renderContent ::
          ( Monad m,
            WithSchema m
          ) =>
          TypeContent bool a VALID ->
          ResolverValue m
        renderContent DataScalar {} = __type KindScalar []
        renderContent (DataEnum enums) = __type KindEnum [("enumValues", render enums)]
        renderContent (DataInputObject inputFields) =
          __type
            KindInputObject
            [("inputFields", render inputFields)]
        renderContent DataObject {objectImplements, objectFields} =
          createObjectType typeName typeDescription objectImplements objectFields
        renderContent (DataUnion union) =
          __type
            KindUnion
            [("possibleTypes", render union)]
        renderContent (DataInputUnion members) =
          mkType
            KindInputObject
            typeName
            ( Just
                ( "Note! This input is an exclusive object,\n"
                    <> "i.e., the customer can provide a value for only one field."
                )
                <> typeDescription
            )
            [ ( "inputFields",
                render (mkInputUnionFields members)
              )
            ]
        renderContent (DataInterface fields) =
          __type
            KindInterface
            [ ("fields", render fields),
              ("possibleTypes", renderPossibleTypes typeName)
            ]

instance RenderIntrospection (UnionMember OUT s) where
  render UnionMember {memberName} = selectType memberName >>= render

instance
  RenderIntrospection (FieldDefinition cat s) =>
  RenderIntrospection (FieldsDefinition cat s)
  where
  render = render . filter fieldVisibility . elems

instance RenderIntrospection (FieldContent TRUE IN VALID) where
  render = render . defaultInputValue

instance RenderIntrospection (Value VALID) where
  render Null = pure mkNull
  render x = pure $ mkString $ fromLBS $ GQL.render x

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
  render = fmap mkList . traverse (render . argument) . elems

instance RenderIntrospection (FieldDefinition IN VALID) where
  render FieldDefinition {..} =
    pure $
      mkObject
        "__InputValue"
        [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          defaultValue fieldContent
        ]

instance RenderIntrospection (DataEnumValue VALID) where
  render DataEnumValue {enumName, enumDescription, enumDirectives} =
    pure $ mkObject "__Field" $
      [ renderName enumName,
        description enumDescription
      ]
        <> renderDeprecated enumDirectives

instance RenderIntrospection TypeRef where
  render TypeRef {typeConName, typeWrappers} = renderWrapper typeWrappers
    where
      renderWrapper :: (Monad m, WithSchema m) => TypeWrapper -> m (ResolverValue m)
      renderWrapper (TypeList nextWrapper isNonNull) =
        pure $ withNonNull isNonNull $
          mkObject
            "__Type"
            [ renderKind KindList,
              ("ofType", renderWrapper nextWrapper)
            ]
      renderWrapper (BaseType isNonNull) =
        withNonNull isNonNull <$> do
          kind <- kindOf <$> selectType typeConName
          pure $ mkType kind typeConName Nothing []

withNonNull ::
  ( Monad m,
    WithSchema m
  ) =>
  Bool ->
  ResolverValue m ->
  ResolverValue m
withNonNull True contentType =
  mkObject
    "__Type"
    [ renderKind KindNonNull,
      ("ofType", pure contentType)
    ]
withNonNull False contentType = contentType

renderPossibleTypes ::
  (Monad m, WithSchema m) =>
  TypeName ->
  m (ResolverValue m)
renderPossibleTypes name =
  mkList
    <$> ( getSchema
            >>= traverse render . possibleInterfaceTypes name
        )

renderDeprecated ::
  ( Monad m,
    WithSchema m
  ) =>
  Directives s ->
  [(FieldName, m (ResolverValue m))]
renderDeprecated dirs =
  [ ("isDeprecated", render (isJust $ lookupDeprecated dirs)),
    ("deprecationReason", render (lookupDeprecated dirs >>= lookupDeprecatedReason))
  ]

description ::
  ( Monad m,
    WithSchema m
  ) =>
  Maybe Description ->
  (FieldName, m (ResolverValue m))
description = ("description",) . render

mkType ::
  ( RenderIntrospection name,
    Monad m,
    WithSchema m
  ) =>
  TypeKind ->
  name ->
  Maybe Description ->
  [(FieldName, m (ResolverValue m))] ->
  ResolverValue m
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
  (Monad m, WithSchema m) =>
  TypeName ->
  Maybe Description ->
  [TypeName] ->
  FieldsDefinition OUT VALID ->
  ResolverValue m
createObjectType name desc interfaces fields =
  mkType (KindObject Nothing) name desc [("fields", render fields), ("interfaces", mkList <$> traverse implementedInterface interfaces)]

implementedInterface ::
  (Monad m, WithSchema m) =>
  TypeName ->
  m (ResolverValue m)
implementedInterface name =
  selectType name
    >>= renderContent
  where
    renderContent typeDef@TypeDefinition {typeContent = DataInterface {}} = render typeDef
    renderContent _ = failure ("Type " <> msg name <> " must be an Interface" :: Message)

renderName ::
  ( RenderIntrospection name,
    Monad m,
    WithSchema m
  ) =>
  name ->
  (FieldName, m (ResolverValue m))
renderName = ("name",) . render

renderKind ::
  (Monad m, WithSchema m) =>
  TypeKind ->
  (FieldName, m (ResolverValue m))
renderKind = ("kind",) . render

type' ::
  (Monad m, WithSchema m) =>
  TypeRef ->
  (FieldName, m (ResolverValue m))
type' = ("type",) . render

defaultValue ::
  (Monad m, WithSchema m) =>
  Maybe (FieldContent TRUE IN VALID) ->
  ( FieldName,
    m (ResolverValue m)
  )
defaultValue = ("defaultValue",) . render
