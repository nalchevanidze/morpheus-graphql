{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.RenderHaskell
  ( renderHaskellDocument
  ) where

import           Data.ByteString.Lazy.Char8              (ByteString)
import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text, intercalate)
import qualified Data.Text                               as T (concat)
import qualified Data.Text.Lazy                          as LT (fromStrict)
import           Data.Text.Lazy.Encoding                 (encodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Document.Rendering.Terms  (renderExtension)
import           Data.Morpheus.Document.Rendering.Types  (renderType)
import           Data.Morpheus.Document.Rendering.Values (renderResolver, renderRootResolver)
import           Data.Morpheus.Types.Internal.Data       (DataTypeLib (..), allDataTypes)

renderHaskellDocument :: DataTypeLib -> ByteString
renderHaskellDocument lib =
  encodeText $ renderLanguageExtensions <> renderExports <> renderImports <> renderRootResolver lib <> types
  where
    encodeText = encodeUtf8 . LT.fromStrict
    types = intercalate "\n\n" $ map (renderType <> const "\n\n" <> renderResolver) (allDataTypes lib)

renderLanguageExtensions :: Text
renderLanguageExtensions = T.concat (map renderExtension extensions) <> "\n"
  where
    extensions = ["OverloadedStrings", "DeriveGeneric", "TypeFamilies"]

renderExports :: Text
renderExports = "-- generated by 'Morpheus' CLI\n" <> "module Schema (rootResolver) where\n\n"

renderImports :: Text
renderImports = T.concat (map renderImport imports) <> "\n"
  where
    renderImport (src, list) = "import  " <> src <> "  (" <> intercalate ", " list <> ")\n"
    --------------------------------------------------------------------------------------
    imports =
      [ ("GHC.Generics", ["Generic"])
      , ("Data.Morpheus.Kind", ["SCALAR", "ENUM", "INPUT_OBJECT", "OBJECT", "UNION"])
      , ("Data.Morpheus.Types", ["GQLRootResolver(..)", "StreamM", "ResM", "GQLType(..)"])
      , ("Data.Text", ["Text"])
      ]
