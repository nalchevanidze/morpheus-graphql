{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Value
  ( validateInputByTypeRef,
    validateInputByType,
    ValidateWithDefault,
  )
where

import Data.Morpheus.Error.Input (typeViolation)
import Data.Morpheus.Error.Variable (incompatibleVariableType)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    singleton,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    DataEnumValue (..),
    DataInputUnion,
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    Message,
    Object,
    ObjectEntry (..),
    Ref (..),
    ScalarDefinition (..),
    ScalarValue (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    Typed (..),
    UnionMember (..),
    VALID,
    ValidValue,
    Value (..),
    Variable (..),
    Variable (..),
    VariableContent (..),
    isNullable,
    isSubtype,
    mkMaybeType,
    mkTypeRef,
    msg,
    msgValidation,
    toCategory,
    toFieldName,
    typed,
    unitFieldName,
    unitTypeName,
    untyped,
    withPosition,
  )
import Data.Morpheus.Types.Internal.Validation
  ( InputContext,
    InputSource (..),
    InputValidator,
    Scope (..),
    Validator,
    askType,
    askTypeMember,
    asksScope,
    constraintInputUnion,
    inField,
    inputMessagePrefix,
    inputValueSource,
    selectKnown,
    selectWithDefaultValue,
    withScopeType,
  )
import Relude

violation ::
  Maybe Message ->
  Value s ->
  InputValidator schemaS ctx a
violation message value = do
  Scope
    { position,
      currentTypeName,
      currentTypeWrappers
    } <-
    asksScope id
  prefix <- inputMessagePrefix
  failure
    $ withPosition position
    $ prefix
      <> typeViolation
        (TypeRef currentTypeName currentTypeWrappers)
        value
      <> maybe "" ((" " <>) . msgValidation) message

checkTypeCompatibility ::
  TypeRef ->
  Ref FieldName ->
  Variable VALID ->
  InputValidator schemaS ctx ValidValue
checkTypeCompatibility valueType ref var@Variable {variableValue = ValidVariableValue value, variableType}
  | variableType `isSubtype` valueType = pure value
  | otherwise = failure $ incompatibleVariableType ref var valueType

validateInputByTypeRef ::
  ValidateWithDefault c schemaS s =>
  Typed IN schemaS TypeRef ->
  Value s ->
  Validator schemaS (InputContext c) (Value VALID)
validateInputByTypeRef
  ref
  value = do
    inputTypeDef <- askType ref
    validateInputByType
      (untyped typeWrappers ref)
      inputTypeDef
      value

validateValueByField ::
  ValidateWithDefault c schemaS s =>
  FieldDefinition IN schemaS ->
  Value s ->
  Validator schemaS (InputContext c) (Value VALID)
validateValueByField field =
  inField field
    . validateInputByTypeRef
      (typed fieldType field)

-- Validate input Values
validateInputByType ::
  ValidateWithDefault ctx schemaS valueS =>
  TypeWrapper ->
  TypeDefinition IN schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
validateInputByType tyWrappers typeDef =
  withScopeType (typeDef, tyWrappers) . validateWrapped tyWrappers typeDef

-- VALIDATION
validateWrapped ::
  ValidateWithDefault ctx schemaS valueS =>
  TypeWrapper ->
  TypeDefinition IN schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
-- Validate Null. value = null ?
validateWrapped wrappers _ (ResolvedVariable ref variable) = do
  typeName <- asksScope currentTypeName
  checkTypeCompatibility (TypeRef typeName wrappers) ref variable
validateWrapped wrappers _ Null
  | isNullable wrappers = pure Null
  | otherwise = violation Nothing Null
-- Validate LIST
validateWrapped (TypeList wrappers _) tyCont (List list) =
  List <$> traverse (validateInputByType wrappers tyCont) list
{-- 2. VALIDATE TYPES, all wrappers are already Processed --}
{-- VALIDATE OBJECT--}
validateWrapped BaseType {} TypeDefinition {typeContent} entryValue =
  validateUnwrapped typeContent entryValue
{-- 3. THROW ERROR: on invalid values --}
validateWrapped _ _ entryValue = violation Nothing entryValue

validateUnwrapped ::
  ValidateWithDefault ctx schemaS valueS =>
  TypeContent TRUE IN schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
validateUnwrapped (DataInputObject parentFields) (Object fields) =
  Object <$> validateInputObject parentFields fields
validateUnwrapped (DataInputUnion inputUnion) (Object rawFields) =
  validatInputUnion inputUnion rawFields
validateUnwrapped (DataEnum tags) value =
  validateEnum tags value
validateUnwrapped (DataScalar dataScalar) value =
  validateScalar dataScalar value
validateUnwrapped _ value = violation Nothing value

-- INPUT UNION
validatInputUnion ::
  ValidateWithDefault ctx schemaS s =>
  DataInputUnion schemaS ->
  Object s ->
  InputValidator schemaS ctx (Value VALID)
validatInputUnion inputUnion rawFields =
  case constraintInputUnion inputUnion rawFields of
    Left message -> violation (Just message) (Object rawFields)
    Right (name, value) -> validatInputUnionMember name value

validatInputUnionMember ::
  ValidateWithDefault ctx schemaS valueS =>
  UnionMember IN schemaS ->
  Value valueS ->
  InputValidator schemaS ctx (Value VALID)
validatInputUnionMember member value = do
  inputDef <- askDef
  mkInputUnionValue member <$> validateInputByType mkMaybeType inputDef value
  where
    askDef
      | nullary member = askType (Typed $ mkTypeRef unitTypeName)
      | otherwise = toCategory <$> askTypeMember member

mkInputUnionValue :: UnionMember IN s' -> Value s -> Value s
mkInputUnionValue
  UnionMember
    { memberName,
      nullary
    } = Object . singleton . ObjectEntry (toFieldName memberName) . packNullary
    where
      packNullary
        | nullary = Object . singleton . ObjectEntry unitFieldName
        | otherwise = id

-- INUT Object
validateInputObject ::
  ValidateWithDefault ctx schemaS valueS =>
  FieldsDefinition IN schemaS ->
  Object valueS ->
  InputValidator schemaS ctx (Object VALID)
validateInputObject fieldsDef object =
  traverse_ (`selectKnown` fieldsDef) object
    *> traverse (validateWithDefault object) fieldsDef

class ValidateWithDefault c schemaS s where
  validateWithDefault ::
    Object s ->
    FieldDefinition IN schemaS ->
    Validator schemaS (InputContext c) (ObjectEntry VALID)

instance ValidateWithDefault c VALID s where
  validateWithDefault object fieldDef@FieldDefinition {fieldName} =
    ObjectEntry fieldName
      <$> selectWithDefaultValue
        pure
        (validateValueByField fieldDef . entryValue)
        fieldDef
        object

instance ValidateWithDefault c CONST s where
  validateWithDefault object fieldDef@FieldDefinition {fieldName} =
    ObjectEntry fieldName
      <$> selectWithDefaultValue
        (validateValueByField fieldDef)
        (validateValueByField fieldDef . entryValue)
        fieldDef
        object

-- Leaf Validations
validateScalar ::
  ScalarDefinition ->
  Value s ->
  InputValidator schemaS ctx ValidValue
validateScalar ScalarDefinition {validateValue} value = do
  typeName <- asksScope currentTypeName
  scalarValue <- toScalar typeName value
  case validateValue scalarValue of
    Right _ -> pure scalarValue
    Left "" -> violation Nothing value
    Left message -> violation (Just $ msg message) value
  where
    toScalar :: TypeName -> Value s -> InputValidator schemaS ctx ValidValue
    toScalar typeName (Scalar x) | isValidDefault typeName x = pure (Scalar x)
    toScalar _ _ = violation Nothing value

isValidDefault :: TypeName -> ScalarValue -> Bool
isValidDefault "Boolean" = isBoolean
isValidDefault "String" = isString
isValidDefault "Float" = oneOf [isFloat, isInt]
isValidDefault "Int" = isInt
isValidDefault "ID" = oneOf [isInt, isFloat, isString]
isValidDefault _ = const True

oneOf :: [a -> Bool] -> a -> Bool
oneOf ls v = any (v &) ls

isBoolean :: ScalarValue -> Bool
isBoolean Boolean {} = True
isBoolean _ = False

isString :: ScalarValue -> Bool
isString String {} = True
isString _ = False

isFloat :: ScalarValue -> Bool
isFloat Float {} = True
isFloat _ = False

isInt :: ScalarValue -> Bool
isInt Int {} = True
isInt _ = False

isVariableValue :: InputValidator schemaS c Bool
isVariableValue =
  \case
    SourceVariable {isDefaultValue} -> not isDefaultValue
    _ -> False
    <$> inputValueSource

validateEnum ::
  [DataEnumValue s] ->
  Value valueS ->
  InputValidator schemaS c ValidValue
validateEnum enumValues value@(Scalar (String enumValue))
  | TypeName enumValue `elem` tags = do
    isFromVariable <- isVariableValue
    if isFromVariable
      then pure (Enum (TypeName enumValue))
      else violation Nothing value
  where
    tags = fmap enumName enumValues
validateEnum enumValues value@(Enum enumValue)
  | enumValue `elem` tags = pure (Enum enumValue)
  | otherwise = violation Nothing value
  where
    tags = fmap enumName enumValues
validateEnum _ value = violation Nothing value
