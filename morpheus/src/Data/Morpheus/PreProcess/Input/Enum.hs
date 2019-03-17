module Data.Morpheus.PreProcess.Input.Enum
  ( validateEnum
  ) where

import           Data.Morpheus.Error.Variable   (invalidEnumOption)
import           Data.Morpheus.Schema.EnumValue (isEnumOf)
import           Data.Morpheus.Schema.Helpers   (Type)
import qualified Data.Morpheus.Schema.Type      as T (enumValues, name)
import           Data.Morpheus.Types.Error      (Validation)
import           Data.Morpheus.Types.JSType     (JSType (..))
import           Data.Morpheus.Types.MetaInfo   (MetaInfo (..))
import           Data.Morpheus.Types.Types      ((::->) (Some), Argument (..))

validateEnum :: Type -> Argument -> Validation Argument
validateEnum _type (Argument (JSEnum argument) pos) =
  if isEnumOf argument (unwrapField $ T.enumValues _type)
    then pure (Argument (JSEnum argument) pos)
    else enumError
  where
    unwrapField (Some x) = x
    enumError = Left $ invalidEnumOption meta
    meta = MetaInfo {typeName = T.name _type, key = argument, position = pos}
