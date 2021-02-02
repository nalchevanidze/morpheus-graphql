{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Fetch
  ( Fetch (..),
    deriveFetch,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON (..),
    eitherDecode,
    encode,
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Client.JSONSchema.Types
  ( JSONResponse (..),
  )
import Data.Morpheus.Internal.TH
  ( applyCons,
    toCon,
    typeInstanceDec,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
  )
import Data.Text
  ( pack,
  )
import Language.Haskell.TH
import Relude hiding (ByteString, Type)

fixVars :: A.Value -> Maybe A.Value
fixVars x
  | x == A.emptyArray = Nothing
  | otherwise = Just x

class Fetch a where
  type Args a :: *
  __fetch ::
    (Monad m, Show a, ToJSON (Args a), FromJSON a) =>
    String ->
    FieldName ->
    (ByteString -> m ByteString) ->
    Args a ->
    m (Either String a)
  __fetch strQuery opName trans vars = (eitherDecode >=> processResponse) <$> trans (encode gqlReq)
    where
      gqlReq = GQLRequest {operationName = Just opName, query = pack strQuery, variables = fixVars (toJSON vars)}
      -------------------------------------------------------------
      processResponse JSONResponse {responseData = Just x, responseErrors = Nothing} = Right x
      processResponse JSONResponse {responseData = Just x, responseErrors = Just []} = Right x
      processResponse invalidResponse = Left (show invalidResponse)
  fetch :: (Monad m, FromJSON a) => (ByteString -> m ByteString) -> Args a -> m (Either String a)

deriveFetch :: Type -> TypeName -> String -> Q [Dec]
deriveFetch resultType typeName queryString =
  pure <$> instanceD (cxt []) iHead methods
  where
    iHead = applyCons ''Fetch [typeName]
    methods =
      [ funD 'fetch [clause [] (normalB [|__fetch queryString typeName|]) []],
        pure $ typeInstanceDec ''Args (toCon typeName) resultType
      ]
