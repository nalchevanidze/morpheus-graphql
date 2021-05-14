{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Terms
  ( name,
    variable,
    varName,
    ignoredTokens,
    parseString,
    collection,
    setOf,
    uniqTuple,
    uniqTupleOpt,
    parseTypeCondition,
    spreadLiteral,
    parseAlias,
    sepByAnd,
    parseName,
    parseType,
    keyword,
    optDescription,
    optionalCollection,
    parseTypeName,
    pipe,
    brackets,
    equal,
    colon,
    at,
    symbol,
  )
where

import Data.ByteString.Lazy.Internal (ByteString)
import Data.Morpheus.Ext.Result (Eventless)
import Data.Morpheus.Internal.Utils
  ( Empty (..),
    FromElems (..),
    KeyOf,
    fromElems,
    fromLBS,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.SourceText
  ( ignoredTokens,
    ignoredTokens1,
    parseStringBS,
  )
import qualified Data.Morpheus.Types.Internal.AST as AST
import Data.Morpheus.Types.Internal.AST
  ( Description,
    FieldName (..),
    Ref (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
  )
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( (<?>),
    between,
    label,
    sepBy,
    sepBy1,
    sepEndBy,
    takeWhile1P,
    takeWhileP,
    try,
  )
import Text.Megaparsec.Byte
  ( char,
    string,
  )

-- ':'
#define COLON 58
-- '@'
#define AT 64
-- '='
#define EQUAL 61
-- '|'
#define PIPE 124
-- '$'
#define DOLLAR 36
-- '&'
#define AMPERSAND 38
-- '_'
#define UNDERSCORE 95
-- '!'
#define BANG 33

#define CHAR_A 65

#define CHAR_Z 90

#define CHAR_a 97

#define CHAR_z 122

#define DIGIT_0 48

#define DIGIT_9 57

symbol :: Word8 -> Parser ()
symbol x = char x *> ignoredTokens
{-# INLINE symbol #-}

colon :: Parser ()
colon = symbol COLON
{-# INLINE colon #-}

at :: Parser ()
at = symbol AT
{-# INLINE at #-}

equal :: Parser ()
equal = symbol EQUAL
{-# INLINE equal #-}

pipe :: Parser a -> Parser [a]
pipe x = optional (symbol PIPE) *> (x `sepBy1` symbol PIPE)
{-# INLINE pipe #-}

-- parens : '()'
parens :: Parser a -> Parser a
parens = between (symbol 40) (symbol 41)
{-# INLINE parens #-}

-- braces: {}
braces :: Parser a -> Parser a
braces = between (symbol 123) (symbol 125)
{-# INLINE braces #-}

-- brackets: []
brackets :: Parser a -> Parser a
brackets = between (symbol 91) (symbol 93)
{-# INLINE brackets #-}

-- 2.1.9 Names
-- https://spec.graphql.org/draft/#Name
-- Name
name :: Parser AST.Token
name =
  label "Name" $
    fromLBS <$> do
      (<>) <$> takeWhile1P Nothing isStartChar <*> takeWhileP Nothing isContinueChar
      <* ignoredTokens
  where
    isStartChar x =
      (x >= CHAR_a && x <= CHAR_z)
        || (x >= CHAR_A && x <= CHAR_Z)
        || x == UNDERSCORE
    {-# INLINE isStartChar #-}
    isContinueChar x =
      isStartChar x
        || (x >= DIGIT_0 && x <= DIGIT_9) -- digit
    {-# INLINE isContinueChar #-}
{-# INLINE name #-}

parseName :: Parser FieldName
parseName = FieldName <$> name
{-# INLINE parseName #-}

parseTypeName :: Parser TypeName
parseTypeName = TypeName <$> name
{-# INLINE parseTypeName #-}

keyword :: ByteString -> Parser ()
keyword x = string x *> ignoredTokens1
{-# INLINE keyword #-}

varName :: Parser FieldName
varName = symbol DOLLAR *> parseName <* ignoredTokens
{-# INLINE varName #-}

-- Variable : https://graphql.github.io/graphql-spec/June2018/#Variable
--
-- Variable :  $Name
--
variable :: Parser (Ref FieldName)
variable =
  label "variable" $
    flip Ref
      <$> getLocation
      <*> varName
{-# INLINE variable #-}

-- Descriptions: https://graphql.github.io/graphql-spec/June2018/#Description
--
-- Description:
--   StringValue
optDescription :: Parser (Maybe Description)
optDescription = optional parseString
{-# INLINE optDescription #-}

parseString :: Parser AST.Token
parseString = label "String" $ fromLBS <$> parseStringBS
{-# INLINE parseString #-}

------------------------------------------------------------------------
sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (optional (symbol AMPERSAND) *> ignoredTokens)
{-# INLINE sepByAnd #-}

-----------------------------
collection :: Parser a -> Parser [a]
collection entry = braces (entry `sepEndBy` ignoredTokens)
{-# INLINE collection #-}

setOf :: (FromElems Eventless a coll, KeyOf k a) => Parser a -> Parser coll
setOf = collection >=> lift . fromElems
{-# INLINE setOf #-}

optionalCollection :: (Empty c) => Parser c -> Parser c
optionalCollection x = x <|> pure empty
{-# INLINE optionalCollection #-}

parseNonNull :: Parser Bool
parseNonNull = (symbol BANG $> True) <|> pure False
{-# INLINE parseNonNull #-}

uniqTuple :: (FromElems Eventless a coll, KeyOf k a) => Parser a -> Parser coll
uniqTuple parser =
  label "Tuple" $
    parens
      (parser `sepBy` ignoredTokens <?> "empty Tuple value!")
      >>= lift . fromElems
{-# INLINE uniqTuple #-}

uniqTupleOpt :: (FromElems Eventless a coll, Empty coll, KeyOf k a) => Parser a -> Parser coll
uniqTupleOpt x = uniqTuple x <|> pure empty
{-# INLINE uniqTupleOpt #-}

-- Type Conditions: https://graphql.github.io/graphql-spec/June2018/#sec-Type-Conditions
--
--  TypeCondition:
--    on NamedType
--
parseTypeCondition :: Parser TypeName
parseTypeCondition = keyword "on" *> parseTypeName
{-# INLINE parseTypeCondition #-}

spreadLiteral :: Parser Position
spreadLiteral = getLocation <* string "..." <* ignoredTokens
{-# INLINE spreadLiteral #-}

-- Field Alias : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Alias
-- Alias
--  Name:
parseAlias :: Parser (Maybe FieldName)
parseAlias = try (optional alias) <|> pure Nothing
  where
    alias = label "alias" (parseName <* colon)
{-# INLINE parseAlias #-}

parseType :: Parser TypeRef
parseType = uncurry TypeRef <$> (unwrapped <|> wrapped)
  where
    unwrapped :: Parser (TypeName, TypeWrapper)
    unwrapped = (,) <$> parseTypeName <*> (BaseType <$> parseNonNull)
    {-# INLINE unwrapped #-}
    ----------------------------------------------
    wrapped :: Parser (TypeName, TypeWrapper)
    wrapped = do
      (typename, wrapper) <- brackets (unwrapped <|> wrapped)
      isRequired <- parseNonNull
      pure (typename, TypeList wrapper isRequired)
    {-# INLINE wrapped #-}
{-# INLINE parseType #-}
