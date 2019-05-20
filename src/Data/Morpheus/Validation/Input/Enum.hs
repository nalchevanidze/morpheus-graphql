{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Validation.Input.Enum
  ( validateEnum
  ) where

import           Data.List                          (elem)
import           Data.Morpheus.Types.Internal.Value (Value (..))
import           Data.Text                          (Text)

validateEnum :: error -> [Text] -> Value -> Either error Value
validateEnum error' tags' (JSEnum enumValue) =
  if enumValue `elem` tags'
    then pure (JSEnum enumValue)
    else Left error'
validateEnum error' _ _ = Left error'
