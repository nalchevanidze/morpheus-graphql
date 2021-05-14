{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.JSONSchema.Types
  ( Introspection (..),
    Schema (..),
    Type (..),
    Field (..),
    InputValue (..),
    EnumValue (..),
    JSONResponse (..),
  )
where

import Data.Aeson
--
-- MORPHEUS
import Data.Morpheus.Client.JSONSchema.TypeKind (TypeKind)
import Data.Morpheus.Client.JSONSchema.TypeRef (TypeRef)
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    TypeName,
  )
import Relude hiding (Type)

-- TYPES FOR DECODING JSON INTROSPECTION
--
newtype Introspection = Introspection
  { __schema :: Schema
  }
  deriving (Generic, Show, FromJSON)

data Schema = Schema
  { types :: [Type],
    queryType :: TypeRef,
    mutationType :: Maybe TypeRef,
    subscriptionType :: Maybe TypeRef
    -- TODO: directives
    --directives: [__Directive]
  }
  deriving (Generic, Show, FromJSON)

data Type = Type
  { kind :: TypeKind,
    name :: Maybe TypeName,
    fields :: Maybe [Field],
    interfaces :: Maybe [Type],
    possibleTypes :: Maybe [Type],
    enumValues :: Maybe [EnumValue],
    inputFields :: Maybe [InputValue],
    ofType :: Maybe Type
  }
  deriving (Generic, Show, FromJSON)

-- FIELD
data Field = Field
  { fieldName :: FieldName,
    fieldArgs :: [InputValue],
    fieldType :: Type
  }
  deriving (Show, Generic)

instance FromJSON Field where
  parseJSON = withObject "Field" objectParser
    where
      objectParser o = Field <$> o .: "name" <*> o .: "args" <*> o .: "type"

-- INPUT
data InputValue = InputValue
  { inputName :: FieldName,
    inputType :: Type
  }
  deriving (Show, Generic)

instance FromJSON InputValue where
  parseJSON = withObject "InputValue" objectParser
    where
      objectParser o = InputValue <$> o .: "name" <*> o .: "type"

-- ENUM
newtype EnumValue = EnumValue
  { enumName :: TypeName
  }
  deriving (Generic, Show)

instance FromJSON EnumValue where
  parseJSON = withObject "EnumValue" objectParser
    where
      objectParser o = EnumValue <$> o .: "name"

instance FromJSON a => FromJSON (JSONResponse a) where
  parseJSON = withObject "JSONResponse" objectParser
    where
      objectParser o =
        JSONResponse <$> o .:? "data" <*> o .:? "errors" .!= []

data JSONResponse a = JSONResponse
  { responseData :: Maybe a,
    responseErrors :: [GQLError]
  }
  deriving (Generic, Show, ToJSON)
