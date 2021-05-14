{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Data.Morpheus.Parsing.Internal.SourceText
  ( parseStringBS,
    ignoredTokens,
    ignoredTokens1,
  )
where

import Data.ByteString.Lazy.Internal
  ( ByteString,
  )
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
  )
import Relude hiding (ByteString, empty, many)
import Text.Megaparsec
  ( choice,
    label,
    many,
    satisfy,
    takeWhile1P,
    takeWhileP,
    unexpected,
  )
import Text.Megaparsec.Byte
  ( char,
    space1,
    string,
  )
import Text.Megaparsec.Error
  ( ErrorItem (..),
  )

-- White Space
#define TABULATION 0x0009
#define NEW_LINE 0x000A
#define SPACE 0x0020
#define CARRIAGE_RETURN 0x000D
#define UNICODE_BOM 0xFEFF
#define NON_CHARACTER 0xFFFF

-- Non-alphabetic characters
#define DOUBLE_QUOTE 34
#define BACKSLASH 92
#define COMMA 44
#define HASH_TAG 35

-- Alphabetic characters
#define CHAR_b 98
#define CHAR_f 102
#define CHAR_n 110
#define CHAR_r 114
#define CHAR_t 116

-- https://spec.graphql.org/June2018/#sec-Source-Text
-- SourceCharacter : [\u0009\u000A\u000D\u0020-\uFFFF]/
isSourceCharacter :: Word8 -> Bool
isSourceCharacter TABULATION = True
isSourceCharacter NEW_LINE = True
isSourceCharacter CARRIAGE_RETURN = True
isSourceCharacter x = SPACE <= x && x <= NON_CHARACTER
{-# INLINE isSourceCharacter #-}

inlineString :: Parser ByteString
inlineString =
  label "String" $
    char DOUBLE_QUOTE
      *> parseContent
      <* ignoredTokens
{-# INLINE inlineString #-}

parseContent :: Parser ByteString
parseContent = do
  xs <- takeWhileP Nothing (\x -> isSourceCharacter x && DOUBLE_QUOTE /= x && x /= BACKSLASH && NEW_LINE /= x)
  z <- satisfy (const True)
  case z of
    DOUBLE_QUOTE -> pure xs
    BACKSLASH -> (xs <>) <$> ((<>) <$> escapeChar <*> parseContent)
    w -> unexpected (Tokens (w :| []))
  where
    escapeChar :: Parser ByteString
    escapeChar =
      choice
        [ char CHAR_b $> "\b",
          char CHAR_f $> "\f",
          char CHAR_n $> "\n",
          char CHAR_r $> "\r",
          char CHAR_t $> "\t",
          char BACKSLASH $> "\\",
          char DOUBLE_QUOTE $> "\"",
          char 47 $> "/"
        ]
    {-# INLINE escapeChar #-}
{-# INLINE parseContent #-}

blockString :: Parser ByteString
blockString = string "\"\"\"" *> content <* ignoredTokens
  where
    content :: Parser ByteString
    content = do
      text <- takeWhileP Nothing (\x -> isSourceCharacter x && x /= DOUBLE_QUOTE)
      doubleQuotes <- takeWhileP Nothing (== DOUBLE_QUOTE)
      case doubleQuotes of
        "\"\"\"" -> pure text
        _ -> ((text <> doubleQuotes) <>) <$> content
    {-# INLINE content #-}
{-# INLINE blockString #-}

parseStringBS :: Parser ByteString
parseStringBS = blockString <|> inlineString
{-# INLINE parseStringBS #-}

-- Ignored Tokens : https://graphql.github.io/graphql-spec/June2018/#sec-Source-Text.Ignored-Tokens
ignoredTokens :: Parser ()
ignoredTokens = label "IgnoredTokens" $ many ignored $> ()
{-# INLINE ignoredTokens #-}

-- isIgnored :: UnicodeBOM, WhiteSpace, LineTerminator, Comment , Comma
ignored :: Parser ()
ignored = (takeWhile1P Nothing isIgnored <|> comment) $> ()
  where
    isIgnored x =
      (x >= TABULATION && x <= CARRIAGE_RETURN)
        || x == SPACE
        || x == COMMA
        || x == UNICODE_BOM
    {-# INLINE isIgnored #-}
    comment = char HASH_TAG *> takeWhileP Nothing (\x -> isSourceCharacter x && x /= NEW_LINE)
    {-# INLINE comment #-}
{-# INLINE ignored #-}

ignoredTokens1 :: Parser ()
ignoredTokens1 = space1 *> ignoredTokens
{-# INLINE ignoredTokens1 #-}
