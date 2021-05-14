{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation
  ( Validator,
    SelectionValidator,
    InputValidator,
    BaseValidator,
    InputSource (..),
    OperationContext (..),
    runValidator,
    DirectiveValidator,
    askType,
    askTypeMember,
    selectRequired,
    selectKnown,
    Constraint (..),
    constraint,
    withScope,
    withScopeType,
    withPosition,
    asks,
    asksScope,
    selectWithDefaultValue,
    startInput,
    inField,
    inputMessagePrefix,
    checkUnused,
    Prop (..),
    constraintInputUnion,
    ScopeKind (..),
    withDirective,
    inputValueSource,
    askVariables,
    Scope (..),
    MissingRequired (..),
    InputContext,
    GetWith,
    SetWith,
    Unknown,
    askSchema,
    askFragments,
    MonadContext,
    CurrentSelection (..),
    getOperationType,
    selectType,
    FragmentValidator,
    askInterfaceTypes,
    validateOptional,
  )
where

import Data.Morpheus.Internal.Utils
  ( Failure (..),
    KeyOf (..),
    Selectable,
    member,
    selectBy,
    selectOr,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    IN,
    Position (..),
    Ref (..),
    TRUE,
    TypeCategory,
    TypeContent (..),
    TypeDefinition (..),
    TypeName (..),
    ValidationError,
    Value (..),
    constraintInputUnion,
    fromAny,
    isNullable,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.Validation.Error
  ( KindViolation (..),
    MissingRequired (..),
    Unknown (..),
    Unused (..),
  )
import Data.Morpheus.Types.Internal.Validation.Internal
  ( askInterfaceTypes,
    askType,
    askTypeMember,
    getOperationType,
  )
import Data.Morpheus.Types.Internal.Validation.Validator
  ( BaseValidator,
    Constraint (..),
    CurrentSelection (..),
    DirectiveValidator,
    FragmentValidator,
    GetWith (..),
    InputContext,
    InputSource (..),
    InputValidator,
    MonadContext (..),
    OperationContext (..),
    Prop (..),
    -- Resolution,
    Scope (..),
    ScopeKind (..),
    SelectionValidator,
    SetWith (..),
    Validator (..),
    ValidatorContext (..),
    askFragments,
    askSchema,
    askVariables,
    asks,
    asksScope,
    inField,
    inputMessagePrefix,
    inputValueSource,
    runValidator,
    startInput,
    withDirective,
    withPosition,
    withScope,
    withScopeType,
  )
import Relude hiding
  ( Constraint,
    asks,
  )

validateOptional :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
validateOptional = traverse

getUnused :: (KeyOf k b, Selectable k a c) => c -> [b] -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf)

failOnUnused :: Unused ctx b => [b] -> Validator s ctx ()
failOnUnused x
  | null x = pure ()
  | otherwise = do
    ctx <- validatorCTX <$> Validator ask
    failure $ fmap (unused ctx) x

checkUnused ::
  ( KeyOf k b,
    Selectable k a ca,
    Unused ctx b
  ) =>
  ca ->
  [b] ->
  Validator s ctx ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  KindViolation k inp =>
  Constraint (k :: TypeCategory) ->
  inp ->
  TypeDefinition ANY s ->
  Validator s ctx (TypeDefinition k s)
constraint IMPLEMENTABLE _ TypeDefinition {typeContent = DataObject {objectFields, ..}, ..} =
  pure TypeDefinition {typeContent = DataObject {objectFields, ..}, ..}
constraint IMPLEMENTABLE _ TypeDefinition {typeContent = DataInterface fields, ..} =
  pure TypeDefinition {typeContent = DataInterface fields, ..}
constraint INPUT ctx x = maybe (failure [kindViolation INPUT ctx]) pure (fromAny x)
constraint target ctx _ = failure [kindViolation target ctx]

selectRequired ::
  ( Selectable FieldName value c,
    MissingRequired c ctx
  ) =>
  Ref FieldName ->
  c ->
  Validator s ctx value
selectRequired selector container =
  do
    ValidatorContext {scope, validatorCTX} <- Validator ask
    selectBy
      [missingRequired scope validatorCTX selector container]
      (keyOf selector)
      container

selectWithDefaultValue ::
  forall ctx values value s validValue.
  ( Selectable FieldName value values,
    MissingRequired values ctx,
    MonadContext (Validator s) s ctx
  ) =>
  (Value s -> Validator s ctx validValue) ->
  (value -> Validator s ctx validValue) ->
  FieldDefinition IN s ->
  values ->
  Validator s ctx validValue
selectWithDefaultValue
  f
  validateF
  field@FieldDefinition
    { fieldName,
      fieldContent
    }
  values =
    selectOr
      (handleNull fieldContent)
      validateF
      fieldName
      values
    where
      ------------------
      handleNull ::
        Maybe (FieldContent TRUE IN s) ->
        Validator s ctx validValue
      handleNull (Just (DefaultInputValue value)) = f value
      handleNull Nothing
        | isNullable field = f Null
        | otherwise = failSelection
      -----------------
      failSelection = do
        ValidatorContext {scope, validatorCTX} <- Validator ask
        position <- asksScope position
        failure [missingRequired scope validatorCTX (Ref fieldName (fromMaybe (Position 0 0) position)) values]

selectType ::
  TypeName ->
  Validator s ctx (TypeDefinition ANY s)
selectType name =
  askSchema >>= selectBy err name
  where
    err = "Unknown Type " <> msgValidation name <> "." :: ValidationError

selectKnown ::
  ( Selectable k a c,
    Unknown c sel ctx,
    KeyOf k sel
  ) =>
  sel ->
  c ->
  Validator s ctx a
selectKnown selector lib =
  do
    ValidatorContext {scope, validatorCTX} <- Validator ask
    selectBy
      (unknown scope validatorCTX lib selector)
      (keyOf selector)
      lib
