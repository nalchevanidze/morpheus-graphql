{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Parsing.JSONSchema.Parse
  ( decodeIntrospection
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy                    (ByteString)
import           Data.Morpheus.Error.Internal            (internalError)
import           Data.Morpheus.Parsing.Internal.Create   (createArgument, createDataTypeLib, createEnumType,
                                                          createField, createScalarType, createType, createUnionType)
import qualified Data.Morpheus.Schema.EnumValue          as E (EnumValue (..))
import qualified Data.Morpheus.Schema.Field              as F (Field (..))
import qualified Data.Morpheus.Schema.InputValue         as I (InputValue (..))
import           Data.Morpheus.Schema.JSONType           (JSONIntro (..), JSONResponse (..), JSONSchema (..),
                                                          JSONType (..))
import           Data.Morpheus.Schema.TypeKind           (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data       (DataFullType (..), DataTypeLib, DataTypeWrapper (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Text                               (Text, pack)

typeFromJSON :: JSONType -> Validation (Text, DataFullType)
typeFromJSON JSONType {name = Just typeName, kind = SCALAR} = pure $ createScalarType typeName
typeFromJSON JSONType {name = Just typeName, kind = ENUM, enumValues = Just enums} =
  pure $ createEnumType typeName (map E.name enums)
typeFromJSON JSONType {name = Just typeName, kind = UNION, possibleTypes = Just unions} =
  case traverse name unions of
    Nothing  -> fail "ERROR: GQL ERROR"
    Just uni -> pure $ createUnionType typeName uni
typeFromJSON JSONType {name = Just typeName, kind = INPUT_OBJECT, inputFields = Just iFields} =
  pure (typeName, InputObject $ createType typeName (map iField iFields))
  where
    iField I.InputValue {I.name = fieldName, I.type' = fType} =
      (fieldName, createField () fieldName (fieldTypeFromJSON fType))
typeFromJSON JSONType {name = Just typeName, kind = OBJECT, fields = Just oFields} =
  pure (typeName, OutputObject $ createType typeName (map oField oFields))
  where
    oField F.Field {F.name = fieldName, F.args = fArgs, F.type' = fType} =
      (fieldName, createField (map genArg fArgs) fieldName (fieldTypeFromJSON fType))
      where
        genArg I.InputValue {I.name = argName, I.type' = argType} = createArgument argName (fieldTypeFromJSON argType)

fieldTypeFromJSON :: JSONType -> ([DataTypeWrapper], Text)
fieldTypeFromJSON = fieldTypeRec []
  where
    fieldTypeRec :: [DataTypeWrapper] -> JSONType -> ([DataTypeWrapper], Text)
    fieldTypeRec acc JSONType {kind = LIST, ofType = Just ofType}     = fieldTypeRec (ListType : acc) ofType
    fieldTypeRec acc JSONType {kind = NON_NULL, ofType = Just ofType} = fieldTypeRec (NonNullType : acc) ofType
    fieldTypeRec acc JSONType {name = Just name}                      = (acc, name)

schemaFromJSON :: [JSONType] -> Validation [(Text, DataFullType)]
schemaFromJSON = traverse typeFromJSON

decodeIntrospection :: ByteString -> Validation DataTypeLib
decodeIntrospection jsonDoc =
  case jsonSchema of
    Left errors -> internalError $ pack errors
    Right JSONResponse {responseData = JSONIntro {__schema = JSONSchema {types}}} ->
      schemaFromJSON types >>= createDataTypeLib
  where
    jsonSchema :: Either String JSONResponse
    jsonSchema = eitherDecode jsonDoc
