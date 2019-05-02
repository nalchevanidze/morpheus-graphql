{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus
  ( interpreter
  ) where

import           Control.Monad.Trans.Except          (ExceptT (..), runExceptT)
import           Data.Aeson                          (decode, encode)
import           Data.ByteString.Lazy.Char8          (ByteString)
import           Data.Morpheus.Error.Utils           (errorMessage, renderErrors)
import           Data.Morpheus.Kind.GQLMutation      (GQLMutation (..))
import           Data.Morpheus.Kind.GQLQuery         (GQLQuery (..))
import           Data.Morpheus.Parser.Parser         (parseGQL, parseLineBreaks)
import           Data.Morpheus.PreProcess.PreProcess (preProcessQuery)
import           Data.Morpheus.Schema.Internal.Types (TypeLib)
import           Data.Morpheus.Types.Error           (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.JSType          (JSType)
import           Data.Morpheus.Types.Query.Operator  (Operator (..))
import           Data.Morpheus.Types.Request         (GQLRequest)
import           Data.Morpheus.Types.Response        (GQLResponse (..))
import           Data.Morpheus.Types.Types           (GQLRoot (..))
import           Data.Text                           (Text, pack)
import           Data.Text.Lazy                      (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding             (decodeUtf8, encodeUtf8)

schema :: (GQLQuery a, GQLMutation b) => a -> b -> TypeLib
schema queryRes mutationRes = mutationSchema mutationRes $ querySchema queryRes

resolve :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> GQLRequest -> ResolveIO JSType
resolve rootResolver body = do
  rootGQL <- ExceptT $ pure (parseGQL body >>= preProcessQuery gqlSchema)
  case rootGQL of
    Query _ _args selection _pos    -> encodeQuery queryRes gqlSchema selection
    Mutation _ _args selection _pos -> encodeMutation mutationRes selection
  where
    gqlSchema = schema queryRes mutationRes
    queryRes = query rootResolver
    mutationRes = mutation rootResolver

lineBreaks :: ByteString -> [Int]
lineBreaks req =
  case decode req of
    Just x  -> parseLineBreaks x
    Nothing -> []

interpreterRaw :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> ByteString -> IO GQLResponse
interpreterRaw rootResolver request = do
  value <- runExceptT $ parseRequest request >>= resolve rootResolver
  case value of
    Left x  -> pure $ Errors $ renderErrors (lineBreaks request) x
    Right x -> pure $ Data x

parseRequest :: ByteString -> ResolveIO GQLRequest
parseRequest text =
  case decode text of
    Just x  -> pure x
    Nothing -> failResolveIO $ errorMessage 0 (pack $ show text)

class Interpreter a where
  interpreter :: (GQLQuery q, GQLMutation m) => GQLRoot q m -> a -> IO a

instance Interpreter ByteString where
  interpreter root request = encode <$> interpreterRaw root request

instance Interpreter Text where
  interpreter root request = toStrict . decodeUtf8 . encode <$> interpreterRaw root (encodeUtf8 $ fromStrict request)
