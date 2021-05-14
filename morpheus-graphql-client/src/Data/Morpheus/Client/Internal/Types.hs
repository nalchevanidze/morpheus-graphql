{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
    ClientDefinition (..),
    ClientConsD,
    FetchError (..)
  )
where

import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldDefinition,
    FieldName,
    TypeKind,
    TypeName,
    VALID,
  )
import Data.Morpheus.Types.Error
  ( GQLError
  )
import Relude

data TypeNameTH = TypeNameTH
  { namespace :: [FieldName],
    typename :: TypeName
  }
  deriving (Show)

type ClientConsD c = ConsD (FieldDefinition c VALID)

data ClientTypeDefinition = ClientTypeDefinition
  { clientTypeName :: TypeNameTH,
    clientCons :: [ClientConsD ANY],
    clientKind :: TypeKind
  }
  deriving (Show)

data ClientDefinition = ClientDefinition
  { clientArguments :: Maybe ClientTypeDefinition,
    clientTypes :: [ClientTypeDefinition]
  }
  deriving (Show)

data FetchError a
  = FetchErrorParseFailure String
  | FetchErrorProducedErrors (NonEmpty GQLError) (Maybe a)
  | FetchErrorNoResult
  deriving (Show, Eq, Generic)
