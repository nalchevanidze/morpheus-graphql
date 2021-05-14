{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client
  ( gql,
    Fetch (..),
    FetchError(..),
    defineQuery,
    defineByDocument,
    defineByDocumentFile,
    defineByDocumentFile',
    defineByIntrospection,
    defineByIntrospectionFile,
    defineByIntrospectionFile',
    ScalarValue (..),
    DecodeScalar (..),
    EncodeScalar (..),
    ID (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
  ( readFile,
  )
import Data.Morpheus.Client.Build
  ( defineQuery,
  )
import Data.Morpheus.Client.Fetch
  ( Fetch (..),
  )
import Data.Morpheus.Client.JSONSchema.Parse
  ( decodeIntrospection,
  )
import Data.Morpheus.Client.Internal.Types
  ( FetchError(..),
  )
import Data.Morpheus.Core
  ( parseFullSchema,
  )
import Data.Morpheus.Internal.Ext
  ( Eventless,
  )
import Data.Morpheus.QuasiQuoter (gql)
import Data.Morpheus.Types.GQLScalar
  ( DecodeScalar (..),
    EncodeScalar (..),
  )
import Data.Morpheus.Types.ID (ID (..))
import Data.Morpheus.Types.Internal.AST
  ( ExecutableDocument,
    ScalarValue (..),
    Schema,
    VALID,
  )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
  ( qAddDependentFile,
  )
import Relude hiding (ByteString)

defineByDocumentFile :: FilePath -> (ExecutableDocument, String) -> Q [Dec]
defineByDocumentFile filePath args = do
  qAddDependentFile filePath
  defineByDocument (L.readFile filePath) args

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByDocumentFile' :: Q FilePath -> (ExecutableDocument, String) -> Q [Dec]
defineByDocumentFile' qFilePath args = qFilePath >>= flip defineByDocumentFile args

defineByIntrospectionFile :: FilePath -> (ExecutableDocument, String) -> Q [Dec]
defineByIntrospectionFile filePath args = do
  qAddDependentFile filePath
  defineByIntrospection (L.readFile filePath) args

-- | This variant exposes 'Q FilePath' enabling the use of TH to generate the 'FilePath'. For example, https://hackage.haskell.org/package/file-embed-0.0.13.0/docs/Data-FileEmbed.html#v:makeRelativeToProject can be used to handle multi package projects more reliably.
defineByIntrospectionFile' :: Q FilePath -> (ExecutableDocument, String) -> Q [Dec]
defineByIntrospectionFile' qFilePath args = qFilePath >>= flip defineByIntrospectionFile args

defineByDocument :: IO ByteString -> (ExecutableDocument, String) -> Q [Dec]
defineByDocument doc = defineQuery (schemaByDocument doc)

schemaByDocument :: IO ByteString -> IO (Eventless (Schema VALID))
schemaByDocument = fmap parseFullSchema

defineByIntrospection :: IO ByteString -> (ExecutableDocument, String) -> Q [Dec]
defineByIntrospection json = defineQuery (decodeIntrospection <$> json)
