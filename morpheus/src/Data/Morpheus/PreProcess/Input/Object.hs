{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Input.Object
  ( validateInputObject
  , validateInputVariable
  ) where

import           Data.Morpheus.PreProcess.Utils      (inputTypeBy)
import           Data.Morpheus.Schema.Internal.Types (InputField (..), InputType (..), TypeLib)
import qualified Data.Morpheus.Schema.Type           as T (kind, name)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Describer       (EnumOf (..))
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           as Text (Text, pack)

typeMismatch :: MetaInfo -> JSType -> Text -> MetaValidation JSType
typeMismatch _ (Scalar (String x)) "String"   = pure (Scalar (String x))
typeMismatch _ (Scalar (Int x)) "Int"         = pure (Scalar (Int x))
typeMismatch _ (Scalar (Boolean x)) "Boolean" = pure (Scalar (Boolean x))
typeMismatch meta isType sType                = Left $ TypeMismatch meta (Text.pack $ show isType) sType

validateFieldType :: MetaInfo -> JSType -> InputType -> MetaValidation JSType
validateFieldType meta jsType type' =
  case (jsType, T.kind type') of
    (Scalar _, EnumOf SCALAR) -> pure jsType -- TODO Validate Scalar
    (_, _)                    -> typeMismatch meta jsType (T.name type')

validateInputObject :: TypeLib -> [(Text, InputField)] -> Position -> (Text, JSType) -> MetaValidation (Text, JSType)
validateInputObject typeLib _parentType pos (_name, JSObject fields) = do
  typeSD <- inputTypeBy pos typeLib _parentType _name
  fieldsQS <- mapM (validateInputObject typeLib typeSD pos) fields
  pure (_name, JSObject fieldsQS)
validateInputObject typeLib _parentType pos (_key, x) =
  inputTypeBy pos typeLib _parentType _key >>= validateFieldType meta x >> pure (_key, x)
  where
    meta = MetaInfo {typeName = T.name _parentType, key = _key, position = pos}

validateInputVariable :: TypeLib -> InputType -> Position -> (Text, JSType) -> MetaValidation JSType
validateInputVariable typeLib _type pos (_, JSObject fields) =
  JSObject <$> mapM (validateInputObject typeLib _type pos) fields
validateInputVariable _ _type pos (varName, x) = validateFieldType meta x _type
  where
    meta = MetaInfo {typeName = "", key = varName, position = pos}
