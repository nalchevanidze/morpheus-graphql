{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
    processParser,
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Ext.Result
  ( Eventless,
    Result (..),
    failure,
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError (..),
    GQLErrors,
    Position (..),
    msg,
  )
import Relude
  ( ($),
    (.),
    Applicative (..),
    Either (..),
    Functor (..),
    Void,
    fst,
  )
import Text.Megaparsec
  ( ParseError,
    ParseErrorBundle
      ( ParseErrorBundle
      ),
    ParsecT,
    SourcePos,
    SourcePos (..),
    attachSourcePos,
    bundleErrors,
    bundlePosState,
    errorOffset,
    getSourcePos,
    parseErrorPretty,
    runParserT,
    unPos,
  )

getLocation :: Parser Position
getLocation = fmap toLocation getSourcePos
{-# INLINEABLE getLocation #-}

toLocation :: SourcePos -> Position
toLocation SourcePos {sourceLine, sourceColumn} =
  Position {line = unPos sourceLine, column = unPos sourceColumn}
{-# INLINEABLE toLocation #-}

type MyError = Void

type Parser = ParsecT MyError ByteString Eventless

type ErrorBundle = ParseErrorBundle ByteString MyError

processParser :: Parser a -> ByteString -> Eventless a
processParser parser txt = case runParserT parser [] txt of
  Success {result} -> case result of
    Right root -> pure root
    Left parseError -> failure (processErrorBundle parseError)
  Failure {errors} -> failure errors

processErrorBundle :: ErrorBundle -> GQLErrors
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors

parseErrorToGQLError :: (ParseError ByteString MyError, SourcePos) -> GQLError
parseErrorToGQLError (err, position) =
  GQLError
    { message = msg (parseErrorPretty err),
      locations = [toLocation position]
    }

bundleToErrors ::
  ErrorBundle -> [(ParseError ByteString MyError, SourcePos)]
bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
  NonEmpty.toList $ fst $
    attachSourcePos
      errorOffset
      bundleErrors
      bundlePosState
