{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parser
  ( parseSchema,
    parseTypeDefinitions,
    parseRequest,
    parseRequestWith,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Ext.Result
  ( Eventless,
    sortErrors,
  )
import Data.Morpheus.Ext.SemigroupM ((<:>))
import Data.Morpheus.Parsing.Document.TypeSystem
  ( parseTypeDefinitions,
  )
import qualified Data.Morpheus.Parsing.Document.TypeSystem as P
  ( parseSchema,
  )
import Data.Morpheus.Parsing.Request.Parser
  ( parseRequest,
  )
import Data.Morpheus.Schema.Schema (internalSchema)
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Operation,
    Schema (..),
    VALID,
  )
import Data.Morpheus.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
  )
import Data.Morpheus.Validation.Document.Validation
  ( validateSchema,
  )
import Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
import Relude hiding (ByteString)

parseSchema ::
  ByteString -> Eventless (Schema VALID)
parseSchema =
  sortErrors
    . ( P.parseSchema
          >=> validateSchema
            True
            Config
              { debug = False,
                validationMode = FULL_VALIDATION
              }
      )

parseRequestWith :: Config -> Schema VALID -> GQLRequest -> Eventless (Operation VALID)
parseRequestWith config schema req = do
  qu <- parseRequest req
  fillSchema <- internalSchema <:> schema
  validateRequest config fillSchema qu
