{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Parser.GraphQL
  ( parse,
    countParsedTypes,
  )
where

import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (toList)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Language.GraphQL.AST as GQL
import Text.Megaparsec (runParser)

countParsedTypes :: Text -> Int
countParsedTypes txt = either (const 0) length (parseTypeDefinitions txt)

parseTypeDefinitions :: Text -> Either Text [GQL.TypeSystemDefinition]
parseTypeDefinitions s =
  case runParser GQL.document "<doc>" s of
    Right (toList -> d) ->
      let tds = [td | GQL.TypeSystemDefinition td _ <- d]
       in if length d == length tds
            then Right tds
            else Left "unexpected query or type system extension"
    Left e ->
      Left (T.pack $ show e)

parse :: Text -> Text
parse x = either (error . show) (T.pack . show) (parseTypeDefinitions x)
