{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Document.Validation
  ( ValidateSchema (..),
  )
where

import Data.Morpheus.Ext.Result
  ( Eventless,
  )
import Data.Morpheus.Ext.SemigroupM
  ( (<:>),
  )
import Data.Morpheus.Schema.Schema
  ( internalSchema,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition (..),
    CONST,
    DataEnumValue (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    IN,
    OUT,
    Schema (..),
    Schema (..),
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    Typed (..),
    UnionMember (..),
    VALID,
    Value,
  )
import Data.Morpheus.Types.Internal.Config (Config (..))
import Data.Morpheus.Types.Internal.Validation
  ( InputSource (..),
    startInput,
    validateOptional,
  )
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    ON_TYPE,
    SchemaValidator,
    TypeEntity (..),
    TypeSystemContext (..),
    inField,
    inType,
    runSchemaValidator,
  )
import Data.Morpheus.Validation.Document.Interface (validateImplements)
import Data.Morpheus.Validation.Internal.Directive
  ( validateDirectives,
  )
import Data.Morpheus.Validation.Internal.Value
  ( validateInputByTypeRef,
  )
import Relude hiding (empty, local)

class ValidateSchema s where
  validateSchema :: Bool -> Config -> Schema s -> Eventless (Schema VALID)

instance ValidateSchema CONST where
  validateSchema withSystem config schema = do
    sysSchema <-
      if withSystem
        then internalSchema <:> schema
        else pure schema
    runSchemaValidator (typeCheck schema) config sysSchema

instance ValidateSchema VALID where
  validateSchema _ _ = pure

----- TypeCheck -------------------------------
---
---
---
class TypeCheck a where
  type TypeContext a :: *
  type TypeContext a = ()
  typeCheck :: a CONST -> SchemaValidator (TypeContext a) (a VALID)

instance TypeCheck Schema where
  typeCheck
    Schema
      { types,
        query,
        mutation,
        subscription,
        directiveDefinitions
      } =
      Schema
        <$> traverse typeCheck types
        <*> typeCheck query
        <*> validateOptional typeCheck mutation
        <*> validateOptional typeCheck subscription
        <*> traverse typeCheck directiveDefinitions

instance TypeCheck (TypeDefinition cat) where
  typeCheck
    TypeDefinition
      { typeName,
        typeDescription,
        typeDirectives,
        typeContent
      } =
      inType typeName $
        TypeDefinition
          typeDescription
          typeName
          <$> validateDirectives (typeDirectiveLocation typeContent) typeDirectives
          <*> typeCheck typeContent

typeDirectiveLocation :: TypeContent a b c -> DirectiveLocation
typeDirectiveLocation DataObject {} = OBJECT
typeDirectiveLocation DataInputObject {} = INPUT_OBJECT
typeDirectiveLocation DataScalar {} = SCALAR
typeDirectiveLocation DataEnum {} = ENUM
typeDirectiveLocation DataInputUnion {} = OBJECT
typeDirectiveLocation DataUnion {} = UNION
typeDirectiveLocation DataInterface {} = INTERFACE

instance TypeCheck (TypeContent TRUE cat) where
  type TypeContext (TypeContent TRUE cat) = TypeEntity ON_TYPE
  typeCheck DataObject {objectImplements, objectFields} =
    DataObject
      <$> validateImplements objectImplements objectFields
      <*> traverse typeCheck objectFields
  typeCheck DataInputObject {inputObjectFields} =
    DataInputObject <$> traverse typeCheck inputObjectFields
  typeCheck DataScalar {..} = pure DataScalar {..}
  typeCheck DataEnum {enumMembers} = DataEnum <$> traverse typeCheck enumMembers
  typeCheck DataInputUnion {inputUnionMembers} =
    DataInputUnion <$> traverse typeCheck inputUnionMembers
  typeCheck DataUnion {unionMembers} = DataUnion <$> traverse typeCheck unionMembers
  typeCheck (DataInterface fields) = DataInterface <$> traverse typeCheck fields

instance FieldDirectiveLocation cat => TypeCheck (FieldDefinition cat) where
  type TypeContext (FieldDefinition cat) = TypeEntity ON_TYPE
  typeCheck FieldDefinition {..} =
    inField
      fieldName
      ( FieldDefinition
          fieldDescription
          fieldName
          fieldType
          <$> validateOptional checkFieldContent fieldContent
          <*> validateDirectives (directiveLocation (Proxy @cat)) fieldDirectives
      )
    where
      checkFieldContent :: FieldContent TRUE cat CONST -> SchemaValidator (Field ON_TYPE) (FieldContent TRUE cat VALID)
      checkFieldContent (FieldArgs args) = FieldArgs <$> traverse typeCheck args
      checkFieldContent (DefaultInputValue value) = DefaultInputValue <$> validateDefaultValue fieldType Nothing value

class FieldDirectiveLocation (cat :: TypeCategory) where
  directiveLocation :: Proxy cat -> DirectiveLocation

instance FieldDirectiveLocation OUT where
  directiveLocation _ = FIELD_DEFINITION

instance FieldDirectiveLocation IN where
  directiveLocation _ = INPUT_FIELD_DEFINITION

instance TypeCheck DirectiveDefinition where
  typeCheck DirectiveDefinition {directiveDefinitionArgs = arguments, ..} =
    inType "Directive" $ inField directiveDefinitionName $ do
      directiveDefinitionArgs <- traverse typeCheck arguments
      pure DirectiveDefinition {..}

instance TypeCheck ArgumentDefinition where
  type TypeContext ArgumentDefinition = Field ON_TYPE
  typeCheck (ArgumentDefinition FieldDefinition {..}) =
    ArgumentDefinition
      <$> ( FieldDefinition
              fieldDescription
              fieldName
              fieldType
              <$> validateOptional checkArgumentDefaultValue fieldContent
              <*> validateDirectives ARGUMENT_DEFINITION fieldDirectives
          )
    where
      checkArgumentDefaultValue (DefaultInputValue value) =
        DefaultInputValue
          <$> validateDefaultValue fieldType (Just fieldName) value

validateDefaultValue ::
  TypeRef ->
  Maybe FieldName ->
  Value CONST ->
  SchemaValidator (Field ON_TYPE) (Value VALID)
validateDefaultValue typeRef argName value = do
  Field fName _ (TypeEntity _ typeName) <- asks local
  startInput (SourceInputField typeName fName argName) (validateInputByTypeRef (Typed typeRef) value)

instance TypeCheck DataEnumValue where
  type TypeContext DataEnumValue = TypeEntity ON_TYPE
  typeCheck DataEnumValue {enumDirectives = directives, ..} =
    DataEnumValue enumDescription enumName
      <$> validateDirectives ENUM_VALUE directives

instance TypeCheck (UnionMember cat) where
  type TypeContext (UnionMember cat) = TypeEntity ON_TYPE
  typeCheck UnionMember {..} = pure UnionMember {..}
