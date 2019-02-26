{-# LANGUAGE OverloadedStrings , DeriveGeneric, DuplicateRecordFields , DeriveAnyClass , DeriveDataTypeable , TypeOperators  #-}

module Data.Morpheus.Types.Introspection
  ( GQL__Type
  , GQL__Field
  , GQL__InputValue
  , GQL__Deprecation__Args
  , GQL__TypeKind(..)
  , createType
  , createField
  , GQLTypeLib
  , emptyLib
  , GQL__EnumValue(..)
  , createInputValue
  , wrapListType
  , unwrapType
  , createScalar
  , createFieldWith
  , createEnum
  )
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import           Data.Map                       ( Map
                                                , fromList
                                                )
import           GHC.Generics
import           Data.Aeson
import           Data.Data                      ( Data )
import           Data.Morpheus.Types.Types      ( (::->)(..) , EnumOf(..) )
import           Data.Morpheus.Schema.GQL__TypeKind
                                                ( GQL__TypeKind(..) )
import           Data.Morpheus.Schema.GQL__EnumValue
                                                ( GQL__EnumValue , createEnumValue)
import            Data.Maybe                     ( fromMaybe )
import qualified  Data.Morpheus.Schema.GQL__InputValue as I (GQL__InputValue(..))
import qualified  Data.Morpheus.Schema.GQL__Field as  F (GQL__Field(..))
import            Data.Morpheus.Schema.GQL__Type  (GQL__Type(..), GQL__Deprecation__Args)

type GQL__InputValue = I.GQL__InputValue GQL__Type;
type GQL__Field =  F.GQL__Field GQL__Type;

createInputValue :: Text -> Text -> I.GQL__InputValue GQL__Type
createInputValue argname typeName = I.GQL__InputValue
  { name         = argname
  , description  = ""
  , _type        = Just $ createType typeName []
  , defaultValue = ""
  }

type GQLTypeLib = Map Text GQL__Type

createField :: Text -> Text -> [I.GQL__InputValue GQL__Type] -> GQL__Field
createField argname typeName args = F.GQL__Field
  { name              = argname
  , description       = "my description"
  , args              = args
  , _type             = Just $ createType typeName []
  , isDeprecated      = False
  , deprecationReason = ""
  }

createFieldWith :: Text -> GQL__Type -> [I.GQL__InputValue GQL__Type] -> GQL__Field
createFieldWith argname fieldtype args = F.GQL__Field
  { name              = argname
  , description       = "my description"
  , args              = args
  , _type             = Just fieldtype
  , isDeprecated      = False
  , deprecationReason = ""
  }

createType :: Text -> [GQL__Field] -> GQL__Type
createType name fields = GQL__Type
  { kind          = EnumOf OBJECT
  , name          = name
  , description   = "my description"
  , fields        = Some fields
  , ofType        = Nothing
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = Some []
  , inputFields   = []
  }

createScalar  :: Text -> GQL__Type
createScalar name  = GQL__Type {
  kind          = EnumOf SCALAR
  , name          = name
  , description   = "my description"
  , fields        = Some []
  , ofType        = Nothing
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = Some []
  , inputFields   = []
}

createEnum  :: Text -> [Text] -> GQL__Type
createEnum name tags = GQL__Type {
  kind          = EnumOf ENUM
  , name          = name
  , description   = "my description"
  , fields        = Some []
  , ofType        = Nothing
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = Some $ map createEnumValue tags
  , inputFields   = []
}


unwrapType :: GQL__Type -> Maybe GQL__Type
unwrapType x = case kind x of
  EnumOf LIST -> ofType x
  _    -> Just x

wrapListType :: GQL__Type -> GQL__Type
wrapListType contentType = GQL__Type
  { kind          = EnumOf LIST
  , name          = ""
  , description   = "list Type"
  , fields        = None
  , ofType        = Just contentType
  , interfaces    = []
  , possibleTypes = []
  , enumValues    = None
  , inputFields   = []
  }

emptyLib :: GQLTypeLib
emptyLib = fromList []