{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Compile
  ( compileSyntax,
    validateWith,
  )
where

--
--  Morpheus

import Data.Morpheus.Client.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Client.Selection
  ( operationTypes,
  )
import Data.Morpheus.Parsing.Internal
  ( parseRequest,
  )
import Data.Morpheus.Types.IO (GQLRequest (..))
import qualified Data.Morpheus.Types.Internal.AST as O
  ( Operation (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ClientQuery (..),
    GQLQuery (..),
    Schema,
    VALIDATION_MODE (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import qualified Data.Text as T
  ( pack,
  )
import Language.Haskell.TH

compileSyntax :: String -> Q Exp
compileSyntax queryText = case parseRequest request of
  Failure errors -> fail (renderGQLErrors errors)
  Success {result, warnings} ->
    gqlWarnings warnings >> [|(result, queryText)|]
  where
    request =
      GQLRequest
        { query = T.pack queryText,
          operationName = Nothing,
          variables = Nothing
        }

validateWith :: Schema -> (GQLQuery, String) -> Eventless ClientQuery
validateWith schema (rawRequest@GQLQuery {operation}, queryText) = do
  validOperation <- validateRequest schema WITHOUT_VARIABLES rawRequest
  (queryArgsType, queryTypes) <-
    operationTypes
      schema
      (O.operationArguments operation)
      validOperation
  return ClientQuery {queryText, queryTypes, queryArgsType}