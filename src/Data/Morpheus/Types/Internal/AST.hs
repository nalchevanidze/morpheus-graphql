{-# LANGUAGE DeriveLift       #-}

module Data.Morpheus.Types.Internal.AST
  ( GQLQuery(..)
  , Variables
  , Key
  , DataScalar
  , DataEnum
  , DataObject
  , DataArgument
  , DataUnion
  , DataArguments
  , DataField(..)
  , DataTyCon(..)
  , DataType(..)
  , DataTypeLib(..)
  , DataTypeWrapper(..)
  , DataValidator(..)
  , DataTypeKind(..)
  , DataFingerprint(..)
  , RawDataType(..)
  , ResolverKind(..)
  , TypeWrapper(..)
  , TypeAlias(..)
  , ArgsType(..)
  , DataEnumValue(..)
  , isTypeDefined
  , initTypeLib
  , defineType
  , isFieldNullable
  , allDataTypes
  , lookupDataType
  , kindOf
  , toNullableField
  , toListField
  , isObject
  , isInput
  , toHSWrappers
  , isNullable
  , toGQLWrapper
  , isWeaker
  , isSubscription
  , isOutputObject
  , sysTypes
  , isDefaultTypeName
  , isSchemaTypeName
  , isPrimitiveTypeName
  , OperationType(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , Name
  , Description
  , isEntNode
  , lookupInputType
  , coerceDataObject
  , getDataType
  , lookupDataObject
  , lookupDataUnion
  , lookupType
  , lookupField
  , lookupUnionTypes
  , lookupSelectionField
  , lookupFieldAsSelectionSet
  , createField
  , createArgument
  , createDataTypeLib
  , createEnumType
  , createScalarType
  , createType
  , createUnionType
  , createAlias
  , createInputUnionFields
  , fieldVisibility
  , Meta(..)
  , Directive(..)
  , createEnumValue
  , fromDataType
  , insertType
  , TypeUpdater
  , lookupDeprecated
  , lookupDeprecatedReason
  , TypeD(..)
  , ConsD(..)
  , ClientQuery(..)
  , GQLTypeD(..)
  , ClientType(..)
  )
where

import           Data.Map                       ( Map )
import           Language.Haskell.TH.Syntax     ( Lift )
import           Instances.TH.Lift              ( )

-- Morpheus
import           Data.Morpheus.Types.Internal.AST.Data


import           Data.Morpheus.Types.Internal.AST.Operation

import           Data.Morpheus.Types.Internal.AST.Selection

import           Data.Morpheus.Types.Internal.AST.Base

import           Data.Morpheus.Types.Internal.AST.Value


type Variables = Map Key Value

data GQLQuery = GQLQuery
  { fragments      :: FragmentLib
  , operation      :: RawOperation
  , inputVariables :: [(Key, Value)]
  } deriving (Show,Lift)
