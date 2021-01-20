{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Document.TypeSystem
  ( parseSchema,
    parseTypeDefinitions,
  )
where

import Data.Foldable (foldr')
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Ext.Result
  ( Eventless,
    failure,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    processParser,
  )
import Data.Morpheus.Parsing.Internal.Pattern
  ( argumentsDefinition,
    enumValueDefinition,
    fieldsDefinition,
    inputFieldsDefinition,
    optionalDirectives,
    parseDirectiveLocation,
    parseOperationType,
    typeDeclaration,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( Term,
    at,
    collection,
    colon,
    equal,
    ignoredTokens,
    keyword,
    optDescription,
    optionalCollection,
    parseName,
    parseTypeName,
    pipe,
    sepByAnd,
    setOf,
  )
import Data.Morpheus.Parsing.Internal.Value
  ( Parse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    Description,
    DirectiveDefinition (..),
    Directives,
    FieldsDefinition,
    OBJECT,
    OUT,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    ScalarDefinition (..),
    Schema,
    SchemaDefinition (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    Value,
    buildSchema,
    mkUnionMember,
    type (<=!),
  )
import Relude hiding (ByteString)
import Text.Megaparsec
  ( Stream,
    Tokens,
    eof,
    label,
    manyTill,
  )

mkObject ::
  (OBJECT <=! a) =>
  Maybe Description ->
  TypeName ->
  [TypeName] ->
  Directives s ->
  FieldsDefinition OUT s ->
  TypeDefinition a s
mkObject typeDescription typeName objectImplements typeDirectives objectFields =
  TypeDefinition
    { typeContent = DataObject {objectImplements, objectFields},
      ..
    }

-- Scalars : https://graphql.github.io/graphql-spec/June2018/#sec-Scalars
--
--  ScalarTypeDefinition:
--    Description(opt) scalar Name Directives(Const)(opt)
--
scalarTypeDefinition ::
  Parse str (Value s) =>
  Maybe Description ->
  Parser str (TypeDefinition ANY s)
scalarTypeDefinition typeDescription =
  label "ScalarTypeDefinition" $
    TypeDefinition typeDescription
      <$> typeDeclaration "scalar"
      <*> optionalDirectives
      <*> pure (DataScalar (ScalarDefinition pure))

-- Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Objects
--
--  ObjectTypeDefinition:
--    Description(opt) type Name ImplementsInterfaces(opt) Directives(Const)(opt) FieldsDefinition(opt)
--
--  ImplementsInterfaces
--    implements &(opt) NamedType
--    ImplementsInterfaces & NamedType
--
--  FieldsDefinition
--    { FieldDefinition(list) }
--
--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
objectTypeDefinition ::
  Parse str (Value s) =>
  Maybe Description ->
  Parser str (TypeDefinition ANY s)
objectTypeDefinition typeDescription =
  label "ObjectTypeDefinition" $
    mkObject typeDescription
      <$> typeDeclaration "type"
      <*> optionalImplementsInterfaces
      <*> optionalDirectives
      <*> fieldsDefinition

-- build object

optionalImplementsInterfaces :: (Stream str, Term str, IsString str) => Parser str [TypeName]
optionalImplementsInterfaces = implements <|> pure []
  where
    implements =
      label "ImplementsInterfaces" $ keyword "implements" *> sepByAnd parseTypeName

-- Interfaces: https://graphql.github.io/graphql-spec/June2018/#sec-Interfaces
--
--  InterfaceTypeDefinition
--    Description(opt) interface Name Directives(Const)(opt) FieldsDefinition(opt)
--
interfaceTypeDefinition ::
  Parse str (Value s) =>
  Maybe Description ->
  Parser str (TypeDefinition ANY s)
interfaceTypeDefinition typeDescription =
  label "InterfaceTypeDefinition" $
    TypeDefinition typeDescription
      <$> typeDeclaration "interface"
      <*> optionalDirectives
      <*> (DataInterface <$> fieldsDefinition)

-- Unions : https://graphql.github.io/graphql-spec/June2018/#sec-Unions
--
--  UnionTypeDefinition:
--    Description(opt) union Name Directives(Const)(opt) UnionMemberTypes(opt)
--
--  UnionMemberTypes:
--    = |(opt) NamedType
--      UnionMemberTypes | NamedType
--
unionTypeDefinition ::
  Parse str (Value s) =>
  Maybe Description ->
  Parser str (TypeDefinition ANY s)
unionTypeDefinition typeDescription =
  label "UnionTypeDefinition" $
    TypeDefinition typeDescription
      <$> typeDeclaration "union"
      <*> optionalDirectives
      <*> (DataUnion <$> unionMemberTypes)
  where
    unionMemberTypes =
      equal
        *> pipe (mkUnionMember <$> parseTypeName)

-- Enums : https://graphql.github.io/graphql-spec/June2018/#sec-Enums
--
--  EnumTypeDefinition
--    Description(opt) enum Name Directives(Const)(opt) EnumValuesDefinition(opt)
--
--  EnumValuesDefinition
--    { EnumValueDefinition(list) }
--
--  EnumValueDefinition
--    Description(opt) EnumValue Directives(Const)(opt)
--
enumTypeDefinition ::
  Parse str (Value s) =>
  Maybe Description ->
  Parser str (TypeDefinition ANY s)
enumTypeDefinition typeDescription =
  label "EnumTypeDefinition" $
    TypeDefinition typeDescription
      <$> typeDeclaration "enum"
      <*> optionalDirectives
      <*> (DataEnum <$> collection enumValueDefinition)

-- Input Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Input-Objects
--
--   InputObjectTypeDefinition
--     Description(opt) input Name  Directives(Const)(opt) InputFieldsDefinition(opt)
--
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputObjectTypeDefinition ::
  Parse str (Value s) =>
  Maybe Description ->
  Parser str (TypeDefinition ANY s)
inputObjectTypeDefinition typeDescription =
  label "InputObjectTypeDefinition" $
    TypeDefinition
      typeDescription
      <$> typeDeclaration "input"
      <*> optionalDirectives
      <*> (DataInputObject <$> inputFieldsDefinition)

-- 3.13 DirectiveDefinition
--
--  DirectiveDefinition:
--     Description[opt] directive @ Name ArgumentsDefinition[opt] repeatable[opt] on DirectiveLocations
--
--  DirectiveLocations:
--    DirectiveLocations | DirectiveLocation
--    |[opt] DirectiveLocation
parseDirectiveDefinition ::
  Parse str (Value s) =>
  Maybe Description ->
  Parser str (DirectiveDefinition s)
parseDirectiveDefinition directiveDefinitionDescription =
  label "DirectiveDefinition" $
    DirectiveDefinition
      <$> ( keyword "directive"
              *> at
              *> parseName
          )
        <*> pure directiveDefinitionDescription
        <*> optionalCollection argumentsDefinition
        <*> (optional (keyword "repeatable") *> keyword "on" *> pipe parseDirectiveLocation)

-- 3.2 Schema
-- SchemaDefinition:
--    schema Directives[Const,opt]
--      { RootOperationTypeDefinition(list) }
--
--  RootOperationTypeDefinition:
--    OperationType: NamedType

-- data SchemaDefinition = SchemaDefinition
--   { query :: TypeName,
--     mutation :: Maybe TypeName,
--     subscription :: Maybe TypeName
--   }
parseSchemaDefinition ::
  ( Stream str,
    Term str,
    IsString str,
    IsString (Tokens str)
  ) =>
  Maybe Description ->
  Parser str SchemaDefinition
parseSchemaDefinition _schemaDescription =
  label "SchemaDefinition" $
    keyword "schema"
      *> ( SchemaDefinition
             <$> optionalDirectives
             <*> setOf parseRootOperationTypeDefinition
         )

parseRootOperationTypeDefinition ::
  ( Stream str,
    Term str,
    IsString str,
    IsString (Tokens str)
  ) =>
  Parser str RootOperationTypeDefinition
parseRootOperationTypeDefinition =
  RootOperationTypeDefinition
    <$> (parseOperationType <* colon)
    <*> parseTypeName

parseTypeSystemUnit ::
  ( Stream str,
    Term str,
    IsString str,
    IsString (Tokens str)
  ) =>
  Parser str RawTypeDefinition
parseTypeSystemUnit =
  label "TypeDefinition" $
    do
      description <- optDescription
      -- scalar | enum |  input | object | union | interface
      parseTypeDef description
        <|> RawSchemaDefinition <$> parseSchemaDefinition description
        <|> RawDirectiveDefinition <$> parseDirectiveDefinition description
  where
    parseTypeDef description =
      RawTypeDefinition
        <$> ( objectTypeDefinition description
                <|> inputObjectTypeDefinition description
                <|> interfaceTypeDefinition description
                <|> unionTypeDefinition description
                <|> enumTypeDefinition description
                <|> scalarTypeDefinition description
            )

typePartition ::
  [RawTypeDefinition] ->
  ( [SchemaDefinition],
    [TypeDefinition ANY CONST],
    [DirectiveDefinition CONST]
  )
typePartition = foldr' split ([], [], [])

split ::
  RawTypeDefinition ->
  ( [SchemaDefinition],
    [TypeDefinition ANY CONST],
    [DirectiveDefinition CONST]
  ) ->
  ( [SchemaDefinition],
    [TypeDefinition ANY CONST],
    [DirectiveDefinition CONST]
  )
split (RawSchemaDefinition schema) (schemas, types, dirs) = (schema : schemas, types, dirs)
split (RawTypeDefinition ty) (schemas, types, dirs) = (schemas, ty : types, dirs)
split (RawDirectiveDefinition dir) (schemas, types, dirs) = (schemas, types, dir : dirs)

--  split (RawDirectiveDefinition d)

withSchemaDefinition ::
  ( [SchemaDefinition],
    [TypeDefinition ANY s],
    [DirectiveDefinition CONST]
  ) ->
  Eventless
    (Maybe SchemaDefinition, [TypeDefinition ANY s], [DirectiveDefinition CONST])
withSchemaDefinition ([], t, dirs) = pure (Nothing, t, dirs)
withSchemaDefinition ([x], t, dirs) = pure (Just x, t, dirs)
withSchemaDefinition (_ : xs, _, _) = failure (fmap nameCollision xs)

parseRawTypeDefinitions ::
  ( Stream str,
    Term str,
    IsString str,
    IsString (Tokens str)
  ) =>
  Parser str [RawTypeDefinition]
parseRawTypeDefinitions =
  label "TypeSystemDefinitions" $
    ignoredTokens
      *> manyTill parseTypeSystemUnit eof

typeSystemDefinition ::
  (Stream str, IsString str, IsString (Tokens str), Term str) =>
  str ->
  Eventless
    ( Maybe SchemaDefinition,
      [TypeDefinition ANY CONST],
      [DirectiveDefinition CONST]
    )
typeSystemDefinition =
  processParser parseRawTypeDefinitions
    >=> withSchemaDefinition . typePartition

parseTypeDefinitions :: (Stream str, IsString str, IsString (Tokens str), Term str) => str -> Eventless [TypeDefinition ANY CONST]
parseTypeDefinitions =
  fmap (\d -> [td | RawTypeDefinition td <- d])
    . processParser parseRawTypeDefinitions

parseSchema :: (Stream str, IsString str, IsString (Tokens str), Term str) => str -> Eventless (Schema CONST)
parseSchema = typeSystemDefinition >=> buildSchema
