{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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
    parseNonNull,
    parseWrappedType,
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

import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (pack)
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
import Data.Morpheus.Parsing.Internal.Literals
  ( at,
    colon,
    equal,
    ignoredTokens,
    ignoredTokens1,
    pipe,
    symbol,
  )
import qualified Data.Morpheus.Types.Internal.AST as AST
import Data.Morpheus.Types.Internal.AST
  ( DataTypeWrapper (..),
    Description,
    FieldName (..),
    Ref (..),
    TypeName (..),
    TypeRef (..),
    toHSWrappers,
  )
import qualified Data.Text as T
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( (<?>),
    between,
    choice,
    label,
    many,
    manyTill,
    sepBy,
    sepEndBy,
    takeWhile1P,
    takeWhileP,
    try,
  )
import Text.Megaparsec.Byte
  ( char,
    digitChar,
    letterChar,
    printChar,
    string,
  )

-- '$'
#define DOLLAR 36
-- '&'
#define AMPERSAND 38
-- '_'
#define UNDERSCORE 95
-- '!'
#define BANG 33
-- '"'
#define DOUBLE_QUOTE 34

-- parens : '()'
parens :: Parser a -> Parser a
parens = between (symbol 40) (symbol 41)
{-# INLINEABLE parens #-}

-- braces: {}
braces :: Parser a -> Parser a
braces = between (symbol 123) (symbol 125)
{-# INLINEABLE braces #-}

-- brackets: []
brackets :: Parser a -> Parser a
brackets = between (symbol 91) (symbol 93)
{-# INLINEABLE brackets #-}

-- 2.1.9 Names
-- https://spec.graphql.org/draft/#Name
-- Name ::
--  NameStart NameContinue[list,opt]
--
name :: Parser AST.Token
name = label "Name" $
  fromLBS <$> do
    nameHead <- takeWhile1P Nothing isDigitOrLetter
    nameTail <- takeWhileP Nothing isDigitOrLetter
    ignoredTokens
    pure (nameHead <> nameTail)
  where
    isDigitOrLetter x =
      x == UNDERSCORE
        || (x >= 65 && x <= 90) -- UpperCase
        || (x >= 97 && x <= 122) --LowerCase
        || (x >= 48 && x <= 57) -- digit
    {-# INLINE isDigitOrLetter #-}
{-# INLINE name #-}

escapedChar :: Parser Char
escapedChar = label "EscapedChar" $ printChar >>= handleEscape
{-# INLINEABLE escapedChar #-}

str :: ByteString -> Parser ()
str x = string x $> ()
{-# INLINEABLE str #-}

parseName :: Parser FieldName
parseName = FieldName <$> name
{-# INLINEABLE parseName #-}

parseTypeName :: Parser TypeName
parseTypeName = TypeName <$> name
{-# INLINEABLE parseTypeName #-}

keyword :: ByteString -> Parser ()
keyword x = string x *> ignoredTokens1
{-# INLINEABLE keyword #-}

varName :: Parser FieldName
varName = symbol DOLLAR *> parseName <* ignoredTokens
{-# INLINEABLE varName #-}

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
{-# INLINEABLE variable #-}

-- Descriptions: https://graphql.github.io/graphql-spec/June2018/#Description
--
-- Description:
--   StringValue
optDescription :: Parser (Maybe Description)
optDescription = optional parseString
{-# INLINEABLE optDescription #-}

parseString :: Parser AST.Token
parseString = blockString <|> inlineString
{-# INLINEABLE parseString #-}

blockString :: Parser AST.Token
blockString = str "\"\"\"" *> (fromLBS <$> content) <* ignoredTokens
  where
    content :: Parser ByteString
    content = do
      text <- takeWhileP Nothing (/= DOUBLE_QUOTE)
      doubleQuotes <- takeWhileP Nothing (== DOUBLE_QUOTE)
      case doubleQuotes of
        "\"\"\"" -> pure text
        _ -> ((text <> doubleQuotes) <>) <$> content
    {-# INLINE content #-}
{-# INLINE blockString #-}

inlineString :: Parser AST.Token
inlineString = stringWith (char DOUBLE_QUOTE) escapedChar
{-# INLINE inlineString #-}

stringWith :: Parser quote -> Parser Char -> Parser AST.Token
stringWith quote parser =
  T.pack
    <$> ( quote
            *> manyTill parser quote
            <* ignoredTokens
        )
{-# INLINE stringWith #-}

handleEscape :: Word8 -> Parser Char
handleEscape 92 = w2c <$> choice escape
handleEscape x = pure (w2c x)
{-# INLINE handleEscape #-}

escape :: [Parser Word8]
escape = escapeCh <$> escapeOptions
  where
    escapeCh :: (Word8, Word8) -> Parser Word8
    escapeCh (code, replacement) = char code $> replacement
    {-# INLINE escapeCh #-}
{-# INLINE escape #-}

escapeOptions :: [(Word8, Word8)]
escapeOptions =
  [ (98, 8),
    (110, 10),
    (102, 12),
    (114, 13),
    (116, 9),
    (92, 92),
    (34, 34),
    (47, 47)
  ]
{-# INLINE escapeOptions #-}

------------------------------------------------------------------------
sepByAnd :: Parser a -> Parser [a]
sepByAnd entry = entry `sepBy` (optional (symbol AMPERSAND) *> ignoredTokens)
{-# INLINEABLE sepByAnd #-}

-----------------------------
collection :: Parser a -> Parser [a]
collection entry = braces (entry `sepEndBy` ignoredTokens)
{-# INLINEABLE collection #-}

setOf :: (FromElems Eventless a coll, KeyOf k a) => Parser a -> Parser coll
setOf = collection >=> lift . fromElems
{-# INLINEABLE setOf #-}

optionalCollection :: (Empty c) => Parser c -> Parser c
optionalCollection x = x <|> pure empty
{-# INLINEABLE optionalCollection #-}

parseNonNull :: Parser [DataTypeWrapper]
parseNonNull = (symbol BANG $> [NonNullType]) <|> pure []
{-# INLINEABLE parseNonNull #-}

uniqTuple :: (FromElems Eventless a coll, KeyOf k a) => Parser a -> Parser coll
uniqTuple parser =
  label "Tuple" $
    parens
      (parser `sepBy` ignoredTokens <?> "empty Tuple value!")
      >>= lift . fromElems
{-# INLINEABLE uniqTuple #-}

uniqTupleOpt :: (FromElems Eventless a coll, Empty coll, KeyOf k a) => Parser a -> Parser coll
uniqTupleOpt x = uniqTuple x <|> pure empty
{-# INLINEABLE uniqTupleOpt #-}

-- Type Conditions: https://graphql.github.io/graphql-spec/June2018/#sec-Type-Conditions
--
--  TypeCondition:
--    on NamedType
--
parseTypeCondition :: Parser TypeName
parseTypeCondition = keyword "on" *> parseTypeName
{-# INLINEABLE parseTypeCondition #-}

spreadLiteral :: Parser Position
spreadLiteral = getLocation <* str "..." <* ignoredTokens
{-# INLINEABLE spreadLiteral #-}

-- Field Alias : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Alias
-- Alias
--  Name:
parseAlias :: Parser (Maybe FieldName)
parseAlias = try (optional alias) <|> pure Nothing
  where
    alias = label "alias" (parseName <* colon)
{-# INLINEABLE parseAlias #-}

parseType :: Parser TypeRef
parseType = parseTypeW <$> parseWrappedType <*> parseNonNull
{-# INLINEABLE parseType #-}

parseTypeW :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> TypeRef
parseTypeW (wrappers, typeConName) nonNull =
  TypeRef
    { typeConName,
      typeWrappers = toHSWrappers (nonNull <> wrappers)
    }
{-# INLINEABLE parseTypeW #-}

parseWrappedType :: Parser ([DataTypeWrapper], TypeName)
parseWrappedType = (unwrapped <|> wrapped) <* ignoredTokens
  where
    unwrapped :: Parser ([DataTypeWrapper], TypeName)
    unwrapped = ([],) <$> parseTypeName <* ignoredTokens
    ----------------------------------------------
    wrapped :: Parser ([DataTypeWrapper], TypeName)
    wrapped = brackets (wrapAsList <$> (unwrapped <|> wrapped) <*> parseNonNull)
{-# INLINEABLE parseWrappedType #-}

wrapAsList :: ([DataTypeWrapper], TypeName) -> [DataTypeWrapper] -> ([DataTypeWrapper], TypeName)
wrapAsList (wrappers, tName) nonNull = (ListType : nonNull <> wrappers, tName)
{-# INLINEABLE wrapAsList #-}
