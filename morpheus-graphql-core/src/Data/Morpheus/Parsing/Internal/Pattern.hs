{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Pattern
  ( inputValueDefinition,
    fieldsDefinition,
    typeDeclaration,
    optionalDirectives,
    enumValueDefinition,
    inputFieldsDefinition,
    parseOperationType,
    argumentsDefinition,
    parseDirectiveLocation,
  )
where

import Data.ByteString.Lazy.Internal (ByteString)
import Data.Morpheus.Parsing.Internal.Arguments
  ( maybeArguments,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( at,
    colon,
    ignoredTokens,
    keyword,
    optDescription,
    parseName,
    parseType,
    parseTypeName,
    setOf,
    uniqTuple,
  )
import Data.Morpheus.Parsing.Internal.Value
  ( Parse (..),
    parseDefaultValue,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition (..),
    ArgumentsDefinition,
    DataEnumValue (..),
    Description,
    Directive (..),
    DirectiveLocation (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    InputFieldsDefinition,
    OUT,
    OperationType (..),
    TRUE,
    TypeName,
    TypeRef,
    Value,
  )
import Relude hiding (ByteString, many)
import Text.Megaparsec
  ( choice,
    label,
    many,
  )
import Text.Megaparsec.Byte (string)

--  EnumValueDefinition: https://graphql.github.io/graphql-spec/June2018/#EnumValueDefinition
--
--  EnumValueDefinition
--    Description(opt) EnumValue Directives(Const)(opt)
--
enumValueDefinition ::
  Parse (Value s) =>
  Parser (DataEnumValue s)
enumValueDefinition =
  label "EnumValueDefinition" $
    DataEnumValue
      <$> optDescription
      <*> parseTypeName
      <*> optionalDirectives
{-# INLINEABLE enumValueDefinition #-}

-- InputValue : https://graphql.github.io/graphql-spec/June2018/#InputValueDefinition
--
-- InputValueDefinition
--   Description(opt) Name : Type DefaultValue(opt) Directives (Const)(opt)
--
inputValueDefinition ::
  Parse (Value s) =>
  Parser (FieldDefinition IN s)
inputValueDefinition =
  label "InputValueDefinition" $
    FieldDefinition
      <$> optDescription
      <*> parseName
      <*> (colon *> parseType)
      <*> optional (DefaultInputValue <$> parseDefaultValue)
      <*> optionalDirectives
{-# INLINEABLE inputValueDefinition #-}

-- Field Arguments: https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
--
-- ArgumentsDefinition:
--   ( InputValueDefinition(list) )
--
argumentsDefinition ::
  Parse (Value s) =>
  Parser (ArgumentsDefinition s)
argumentsDefinition =
  label "ArgumentsDefinition" $
    uniqTuple (fmap ArgumentDefinition inputValueDefinition)
{-# INLINEABLE argumentsDefinition #-}

--  FieldsDefinition : https://graphql.github.io/graphql-spec/June2018/#FieldsDefinition
--
--  FieldsDefinition :
--    { FieldDefinition(list) }
--
fieldsDefinition ::
  Parse (Value s) =>
  Parser (FieldsDefinition OUT s)
fieldsDefinition = label "FieldsDefinition" $ setOf fieldDefinition
{-# INLINEABLE fieldsDefinition #-}

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
fieldDefinition :: Parse (Value s) => Parser (FieldDefinition OUT s)
fieldDefinition =
  label "FieldDefinition" $
    mkField
      <$> optDescription
      <*> parseName
      <*> optional (FieldArgs <$> argumentsDefinition)
      <*> (colon *> parseType)
      <*> optionalDirectives
{-# INLINEABLE fieldDefinition #-}

mkField ::
  Maybe Description ->
  FieldName ->
  Maybe (FieldContent TRUE cat s) ->
  TypeRef ->
  [Directive s] ->
  FieldDefinition cat s
mkField fieldDescription fieldName fieldContent fieldType fieldDirectives =
  FieldDefinition {..}
{-# INLINEABLE mkField #-}

-- InputFieldsDefinition : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
--   InputFieldsDefinition:
--     { InputValueDefinition(list) }
--
inputFieldsDefinition ::
  Parse (Value s) =>
  Parser (InputFieldsDefinition s)
inputFieldsDefinition = label "InputFieldsDefinition" $ setOf inputValueDefinition
{-# INLINEABLE inputFieldsDefinition #-}

-- Directives : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Directives
--
-- example: @directive ( arg1: "value" , .... )
--
-- Directives[Const]
-- Directive[Const](list)
--
optionalDirectives :: Parse (Value s) => Parser [Directive s]
optionalDirectives = label "Directives" $ many directive
{-# INLINEABLE optionalDirectives #-}

-- Directive[Const]
--
-- @ Name Arguments[Const](opt)
directive :: Parse (Value s) => Parser (Directive s)
directive =
  label "Directive" $
    Directive
      <$> getLocation
      <*> (at *> parseName)
      <*> maybeArguments
{-# INLINEABLE directive #-}

-- typDeclaration : Not in spec ,start part of type definitions
--
--  typDeclaration
--   Description(opt) scalar Name
--
typeDeclaration :: ByteString -> Parser TypeName
typeDeclaration kind = keyword kind *> parseTypeName
{-# INLINEABLE typeDeclaration #-}

parseOperationType :: Parser OperationType
parseOperationType =
  label "OperationType" $
    ( (string "query" $> Query)
        <|> (string "mutation" $> Mutation)
        <|> (string "subscription" $> Subscription)
    )
      <* ignoredTokens
{-# INLINEABLE parseOperationType #-}

parseDirectiveLocation :: Parser DirectiveLocation
parseDirectiveLocation =
  label
    "DirectiveLocation"
    ( choice $
        toKeyword
          <$> [ FIELD_DEFINITION,
                FRAGMENT_DEFINITION,
                FRAGMENT_SPREAD,
                INLINE_FRAGMENT,
                ARGUMENT_DEFINITION,
                INTERFACE,
                ENUM_VALUE,
                INPUT_OBJECT,
                INPUT_FIELD_DEFINITION,
                SCHEMA,
                SCALAR,
                OBJECT,
                QUERY,
                MUTATION,
                SUBSCRIPTION,
                UNION,
                ENUM,
                FIELD
              ]
    )
    <* ignoredTokens
{-# INLINEABLE parseDirectiveLocation #-}

toKeyword :: Show a => a -> Parser a
toKeyword x = string (fromString $ show x) $> x
{-# INLINEABLE toKeyword #-}
