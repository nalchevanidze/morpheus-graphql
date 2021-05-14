{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Value
  ( enumValue,
    parseDefaultValue,
    Parse (..),
  )
where

import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( brackets,
    colon,
    equal,
    ignoredTokens,
    parseName,
    parseString,
    parseTypeName,
    setOf,
    symbol,
    variable,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    FieldName,
    ObjectEntry (..),
    OrdMap,
    RAW,
    ScalarValue (..),
    Value (..),
    decodeScientific,
  )
import Relude
import Text.Megaparsec
  ( label,
    sepBy,
  )
import Text.Megaparsec.Byte
  ( string,
  )
import Text.Megaparsec.Byte.Lexer (scientific)

-- '-'
#define MINUS 45

valueNull :: Parser (Value a)
valueNull = string "null" $> Null
{-# INLINE valueNull #-}

booleanValue :: Parser (Value a)
booleanValue =
  Scalar . Boolean
    <$> ( string "true" $> True
            <|> string "false" $> False
        )
{-# INLINE booleanValue #-}

valueNumber :: Parser (Value a)
valueNumber = Scalar . decodeScientific <$> ((*) <$> negation <*> scientific)
  where
    negation = (symbol MINUS $> (-1) <* ignoredTokens) <|> pure 1
    {-# INLINE negation #-}
{-# INLINE valueNumber #-}

enumValue :: Parser (Value a)
enumValue = Enum <$> parseTypeName <* ignoredTokens
{-# INLINE enumValue #-}

stringValue :: Parser (Value a)
stringValue = Scalar . String <$> parseString
{-# INLINE stringValue #-}

listValue :: Parser a -> Parser [a]
listValue parser = label "List" $ brackets (parser `sepBy` ignoredTokens)
{-# INLINE listValue #-}

objectEntry :: Parser (Value a) -> Parser (ObjectEntry a)
objectEntry parser = ObjectEntry <$> (parseName <* colon) <*> parser
{-# INLINE objectEntry #-}

objectValue :: Parser (Value a) -> Parser (OrdMap FieldName (ObjectEntry a))
objectValue = label "ObjectValue" . setOf . objectEntry
{-# INLINE objectValue #-}

parsePrimitives :: Parser (Value a)
parsePrimitives =
  valueNull
    <|> booleanValue
    <|> valueNumber
    <|> enumValue
    <|> stringValue
{-# INLINE parsePrimitives #-}

parseDefaultValue :: Parser (Value s)
parseDefaultValue = equal *> parseV
  where
    parseV :: Parser (Value s)
    parseV = structValue parseV

class Parse a where
  parse :: Parser a

instance Parse (Value RAW) where
  parse = (VariableValue <$> variable) <|> structValue parse

instance Parse (Value CONST) where
  parse = structValue parse

structValue :: Parser (Value a) -> Parser (Value a)
structValue parser =
  label "Value" $
    ( parsePrimitives
        <|> (Object <$> objectValue parser)
        <|> (List <$> listValue parser)
    )
      <* ignoredTokens
