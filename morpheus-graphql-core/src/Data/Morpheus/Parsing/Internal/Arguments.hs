{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Arguments (maybeArguments) where

-- MORPHEUS
import Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Data.Morpheus.Parsing.Internal.Terms
  ( colon,
    parseName,
    uniqTupleOpt,
  )
import Data.Morpheus.Parsing.Internal.Value
  ( Parse (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    Value,
  )
import Relude
import Text.Megaparsec (label)

-- Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Language.Arguments
--
-- Arguments[Const]
-- ( Argument[Const](list) )
--
-- Argument[Const]
--  Name : Value[Const]
valueArgument :: Parse (Value s) => Parser (Argument s)
valueArgument =
  label "Argument" $
    Argument
      <$> getLocation
      <*> (parseName <* colon)
      <*> parse
{-# INLINEABLE valueArgument #-}

maybeArguments :: Parse (Value s) => Parser (Arguments s)
maybeArguments =
  label "Arguments" $
    uniqTupleOpt valueArgument
{-# INLINEABLE maybeArguments #-}
