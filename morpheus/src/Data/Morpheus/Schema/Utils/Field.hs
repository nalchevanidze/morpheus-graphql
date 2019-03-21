{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Schema.Utils.Field
  ( fieldByKey
  , argsByKey
  , wrapAsListType
  ) where

import           Data.List                        (find)
import qualified Data.Morpheus.Schema.Field       as F (Field (..))
import qualified Data.Morpheus.Schema.Type        as T (Type (..))
import           Data.Morpheus.Schema.Utils.Utils (Field, InputValue, Type, wrapListType)
import           Data.Morpheus.Types.Describer    (unpackDeprecationArgs)
import           Data.Text                        (Text)

fieldByKey :: Text -> Type -> Maybe Field
fieldByKey key gqlType = find (\x -> key == F.name x) $ unpackDeprecationArgs $ T.fields gqlType

argsByKey :: Text -> Type -> Maybe [InputValue]
argsByKey key gqlType = F.args <$> fieldByKey key gqlType

wrapAsListType :: Field -> Field
wrapAsListType x = x {F._type = wrapListType <$> F._type x}
