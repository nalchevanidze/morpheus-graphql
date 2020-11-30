{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST
  ( Ref (..),
    Position (..),
    Message,
    anonymousRef,
    FieldName (..),
    Description,
    Stage,
    CONST,
    VALID,
    RAW,
    Value (..),
    ScalarValue (..),
    Object,
    GQLValue (..),
    replaceValue,
    decodeScientific,
    convertToJSONName,
    convertToHaskellName,
    RawValue,
    ValidValue,
    RawObject,
    ValidObject,
    ResolvedObject,
    ResolvedValue,
    splitDuplicates,
    removeDuplicates,
    Argument (..),
    Arguments,
    SelectionSet,
    SelectionContent (..),
    Selection (..),
    Fragments,
    Fragment (..),
    Operation (..),
    Variable (..),
    VariableDefinitions,
    DefaultValue,
    getOperationName,
    ScalarDefinition (..),
    DataEnum,
    FieldsDefinition,
    ArgumentDefinition,
    DataUnion,
    ArgumentsDefinition (..),
    FieldDefinition (..),
    InputFieldsDefinition,
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    DataTypeWrapper (..),
    TypeKind (..),
    TypeWrapper (..),
    TypeRef (..),
    DataEnumValue (..),
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    Directive (..),
    ConsD (..),
    TypeCategory (..),
    DataInputUnion,
    VariableContent (..),
    TypeLib,
    initTypeLib,
    kindOf,
    toNullable,
    isObject,
    toHSWrappers,
    isNullable,
    toGQLWrapper,
    isWeaker,
    isSubscription,
    isOutputObject,
    isNotSystemTypeName,
    isEntNode,
    mkEnumContent,
    createScalarType,
    mkUnionContent,
    mkTypeRef,
    mkInputUnionFields,
    fieldVisibility,
    lookupDeprecated,
    lookupDeprecatedReason,
    lookupWith,
    hsTypeName,
    GQLQuery (..),
    Variables,
    unsafeFromFields,
    OrdMap (..),
    GQLError (..),
    GQLErrors,
    ObjectEntry (..),
    UnionTag (..),
    ANY,
    IN,
    OUT,
    OBJECT,
    IMPLEMENTABLE,
    fromAny,
    toAny,
    TRUE,
    FALSE,
    TypeName (..),
    Token,
    Msg (..),
    intercalateName,
    toFieldName,
    TypeNameRef (..),
    isEnum,
    fieldsToArguments,
    mkCons,
    mkConsEnum,
    Directives,
    DirectiveDefinitions,
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    fieldContentArgs,
    mkInputValue,
    mkType,
    TypeNameTH (..),
    isOutput,
    mkObjectField,
    UnionMember (..),
    mkUnionMember,
    mkNullaryMember,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    UnionSelection,
    SchemaDefinition (..),
    buildSchema,
    InternalError (..),
    ValidationError (..),
    msgInternal,
    getOperationDataType,
    Typed (Typed),
    typed,
    untyped,
    msgValidation,
    withPosition,
    ValidationErrors,
    toGQLError,
    LEAF,
    INPUT_OBJECT,
    ToCategory (..),
    FromCategory (..),
    possibleTypes,
    possibleInterfaceTypes,
    mkField,
    safeDefineType,
    defineSchemaWith,
    type (<=!),
    ToOBJECT,
  )
where

import Data.HashMap.Lazy (HashMap)
import Data.Morpheus.Ext.OrdMap
import Data.Morpheus.Types.Internal.AST.Base
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation (..))
import Data.Morpheus.Types.Internal.AST.Fields
import Data.Morpheus.Types.Internal.AST.Selection
import Data.Morpheus.Types.Internal.AST.Stage
import Data.Morpheus.Types.Internal.AST.TH
import Data.Morpheus.Types.Internal.AST.TypeCategory
import Data.Morpheus.Types.Internal.AST.TypeSystem
import Data.Morpheus.Types.Internal.AST.Value
import Language.Haskell.TH.Syntax (Lift)
import Prelude (Show)

type Variables = HashMap FieldName ResolvedValue

data GQLQuery = GQLQuery
  { inputVariables :: [(FieldName, ResolvedValue)],
    operation :: Operation RAW,
    fragments :: Fragments RAW
  }
  deriving (Show, Lift)
