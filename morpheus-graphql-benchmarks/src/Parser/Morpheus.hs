{-# LANGUAGE OverloadedStrings #-}

module Parser.Morpheus
  ( parse,
    countParsedTypes,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Core (parseTypeDefinitions)
import Data.Morpheus.Internal.Ext (resultOr)
import Data.Text (Text)

countParsedTypes :: ByteString -> Int
countParsedTypes bs = resultOr (const 0) length (parseTypeDefinitions bs)

parse :: ByteString -> ByteString
parse x = resultOr (error . show) (const "OK") (parseTypeDefinitions x)
