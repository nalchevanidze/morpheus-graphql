{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.Validation.Validator
  ( Validator (..),
    SelectionValidator,
    InputValidator,
    BaseValidator,
    runValidator,
    Constraint (..),
    withScope,
    withScopeType,
    withPosition,
    inField,
    inputMessagePrefix,
    InputSource (..),
    InputContext (..),
    OperationContext (..),
    CurrentSelection (..),
    renderInputPrefix,
    Prop (..),
    --  Resolution,
    ScopeKind (..),
    inputValueSource,
    Scope (..),
    withDirective,
    startInput,
    GetWith (..),
    SetWith (..),
    MonadContext (..),
    withContext,
    renderField,
    asks,
    asksScope,
    askSchema,
    askVariables,
    askFragments,
    DirectiveValidator,
    ValidatorContext (..),
    FragmentValidator,
  )
where

import Data.Morpheus.Error.Utils
  ( validationErrorMessage,
  )
import Data.Morpheus.Ext.Result
  ( Eventless,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL,
    render,
  )
import Data.Morpheus.Types.Internal.AST
  ( Directive (..),
    FieldDefinition (..),
    FieldName (..),
    Fragments,
    GQLError (..),
    GQLErrors,
    IMPLEMENTABLE,
    IN,
    InternalError,
    Message,
    Position,
    RAW,
    Ref (..),
    Schema,
    Stage,
    TypeCategory,
    TypeDefinition (..),
    TypeKind (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper,
    VALID,
    ValidationError (..),
    Variable (..),
    VariableDefinitions,
    intercalateName,
    kindOf,
    msg,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.Config (Config (..))
import Relude hiding
  ( Constraint,
    asks,
    get,
  )

data Prop = Prop
  { propName :: FieldName,
    propTypeName :: TypeName
  }
  deriving (Show)

type Path = [Prop]

renderPath :: Path -> ValidationError
renderPath [] = ""
renderPath path = "in field " <> msgValidation (intercalateName "." $ fmap propName path) <> ": "

renderInputPrefix :: InputContext c -> ValidationError
renderInputPrefix InputContext {inputPath, inputSource} =
  renderSource inputSource <> renderPath inputPath

renderSource :: InputSource -> ValidationError
renderSource (SourceArgument argumentName) =
  "Argument " <> msgValidation argumentName <> " got invalid value. "
renderSource (SourceVariable Variable {variableName} _) =
  "Variable " <> msgValidation ("$" <> variableName) <> " got invalid value. "
renderSource SourceInputField {sourceTypeName, sourceFieldName, sourceArgumentName} =
  "Field " <> renderField sourceTypeName sourceFieldName sourceArgumentName <> " got invalid default value. "

renderField :: TypeName -> FieldName -> Maybe FieldName -> ValidationError
renderField (TypeName tname) (FieldName fname) arg =
  msgValidation (tname <> "." <> fname <> renderArg arg)
  where
    renderArg (Just (FieldName argName)) = "(" <> argName <> ":)"
    renderArg Nothing = ""

data ScopeKind
  = DIRECTIVE
  | SELECTION
  | TYPE
  deriving (Show)

data
  OperationContext
    (s1 :: Stage)
    (s2 :: Stage) = OperationContext
  { fragments :: Fragments s2,
    variables :: VariableDefinitions s1,
    selection :: CurrentSelection
  }
  deriving (Show)

newtype CurrentSelection = CurrentSelection
  { operationName :: Maybe FieldName
  }
  deriving (Show)

data Scope = Scope
  { position :: Maybe Position,
    currentTypeName :: TypeName,
    currentTypeKind :: TypeKind,
    currentTypeWrappers :: TypeWrapper,
    fieldname :: FieldName,
    kind :: ScopeKind
  }
  deriving (Show)

data InputContext ctx = InputContext
  { inputSource :: InputSource,
    inputPath :: [Prop],
    sourceContext :: ctx
  }
  deriving (Show)

data InputSource
  = SourceArgument FieldName
  | SourceVariable
      { sourceVariable :: Variable RAW,
        isDefaultValue :: Bool
      }
  | SourceInputField
      { sourceTypeName :: TypeName,
        sourceFieldName :: FieldName,
        sourceArgumentName :: Maybe FieldName
      }
  deriving (Show)

data Constraint (a :: TypeCategory) where
  IMPLEMENTABLE :: Constraint IMPLEMENTABLE
  INPUT :: Constraint IN

inField :: FieldDefinition IN s -> InputValidator s c a -> InputValidator s c a
inField
  FieldDefinition
    { fieldName,
      fieldType = TypeRef {typeConName}
    } = withContext update
    where
      update
        InputContext
          { inputPath = old,
            ..
          } =
          InputContext
            { inputPath = old <> [Prop fieldName typeConName],
              ..
            }

inputValueSource ::
  forall m c s.
  ( GetWith c InputSource,
    MonadContext m s c
  ) =>
  m c InputSource
inputValueSource = get

asks ::
  ( MonadContext m s c,
    GetWith c t
  ) =>
  (t -> a) ->
  m c a
asks f = f <$> get

asksScope ::
  ( MonadContext m s c
  ) =>
  (Scope -> a) ->
  m c a
asksScope f = f <$> getGlobalContext scope

setSelectionName ::
  (MonadContext m s c) =>
  FieldName ->
  m c a ->
  m c a
setSelectionName fieldname = setScope update
  where
    update ctx = ctx {fieldname}

askSchema ::
  ( MonadContext m s c
  ) =>
  m c (Schema s)
askSchema = getGlobalContext schema

askVariables ::
  ( MonadContext m s c,
    GetWith c (VariableDefinitions VALID)
  ) =>
  m c (VariableDefinitions VALID)
askVariables = get

askFragments ::
  ( MonadContext m s c,
    GetWith c (Fragments s')
  ) =>
  m c (Fragments s')
askFragments = get

runValidator :: Validator s ctx a -> Config -> Schema s -> Scope -> ctx -> Eventless a
runValidator (Validator x) config schema scope validatorCTX =
  runReaderT x ValidatorContext {..}

withContext ::
  (c' -> c) ->
  Validator s c a ->
  Validator s c' a
withContext f = Validator . withReaderT (fmap f) . _runValidator

withDirective ::
  ( MonadContext m schemaS c
  ) =>
  Directive s ->
  m c a ->
  m c a
withDirective
  Directive
    { directiveName,
      directivePosition
    } = setSelectionName directiveName . setScope update
    where
      update Scope {..} =
        Scope
          { position = Just directivePosition,
            kind = DIRECTIVE,
            ..
          }

withScope ::
  ( MonadContext m s c
  ) =>
  TypeDefinition cat s ->
  Ref FieldName ->
  m c a ->
  m c a
withScope t@TypeDefinition {typeName} (Ref selName pos) =
  setSelectionName selName . setScope update
  where
    update Scope {..} =
      Scope
        { currentTypeName = typeName,
          currentTypeKind = kindOf t,
          position = Just pos,
          ..
        }

withScopeType ::
  ( MonadContext m s c
  ) =>
  (TypeDefinition cat s, TypeWrapper) ->
  m c a ->
  m c a
withScopeType (t@TypeDefinition {typeName}, wrappers) = setScope update
  where
    update Scope {..} =
      Scope
        { currentTypeName = typeName,
          currentTypeKind = kindOf t,
          currentTypeWrappers = wrappers,
          ..
        }

withPosition ::
  ( MonadContext m s c
  ) =>
  Position ->
  m c a ->
  m c a
withPosition pos = setScope update
  where
    update Scope {..} = Scope {position = Just pos, ..}

inputMessagePrefix :: InputValidator s ctx ValidationError
inputMessagePrefix =
  renderInputPrefix
    . validatorCTX <$> Validator ask

startInput ::
  InputSource ->
  InputValidator s ctx a ->
  Validator s ctx a
startInput inputSource = withContext update
  where
    update sourceContext =
      InputContext
        { inputSource,
          inputPath = [],
          sourceContext
        }

data ValidatorContext (s :: Stage) (ctx :: *) = ValidatorContext
  { scope :: Scope,
    schema :: Schema s,
    validatorCTX :: ctx,
    config :: Config
  }
  deriving (Show, Functor)

newtype Validator s ctx a = Validator
  { _runValidator ::
      ReaderT
        (ValidatorContext s ctx)
        Eventless
        a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad
    )

instance MonadReader ctx (Validator s ctx) where
  ask = validatorCTX <$> Validator ask
  local = withContext

type BaseValidator = Validator VALID (OperationContext RAW RAW)

type FragmentValidator (s :: Stage) = Validator VALID (OperationContext VALID s)

type SelectionValidator = Validator VALID (OperationContext VALID VALID)

type InputValidator s ctx = Validator s (InputContext ctx)

type DirectiveValidator ctx = Validator ctx

setScope ::
  (MonadContext m s c) =>
  (Scope -> Scope) ->
  m c b ->
  m c b
setScope f = setGlobalContext (mapScope f)

mapScope :: (Scope -> Scope) -> ValidatorContext s ctx -> ValidatorContext s ctx
mapScope f ValidatorContext {scope, ..} = ValidatorContext {scope = f scope, ..}

-- Helpers
get :: (MonadContext m s ctx, GetWith ctx a) => m ctx a
get = getContext getWith

class
  Monad (m c) =>
  MonadContext m s c
    | m -> s
  where
  getGlobalContext :: (ValidatorContext s c -> a) -> m c a
  setGlobalContext :: (ValidatorContext s c -> ValidatorContext s c) -> m c b -> m c b
  getContext :: (c -> a) -> m c a
  setContext :: (c -> c) -> m c b -> m c b

instance MonadContext (Validator s) s c where
  getGlobalContext f = f <$> Validator ask
  getContext f = f . validatorCTX <$> Validator ask
  setGlobalContext f = Validator . withReaderT f . _runValidator
  setContext = withContext

class GetWith (c :: *) (v :: *) where
  getWith :: c -> v

instance GetWith (OperationContext VALID fragStage) (VariableDefinitions VALID) where
  getWith = variables

instance GetWith (InputContext ctx) InputSource where
  getWith = inputSource

instance GetWith (OperationContext varStage fragStage) (Fragments fragStage) where
  getWith = fragments

-- Setters
class SetWith (c :: *) (v :: *) where
  setWith :: (v -> v) -> c -> c

instance SetWith (OperationContext s1 s2) CurrentSelection where
  setWith f OperationContext {selection = selection, ..} =
    OperationContext
      { selection = f selection,
        ..
      }

instance Failure [ValidationError] (Validator s ctx) where
  failure errors = do
    ctx <- Validator ask
    failValidator (fmap (fromValidatinError ctx) errors)

instance Failure ValidationError (Validator s ctx) where
  failure err = failure [err]

failValidator :: GQLErrors -> Validator s ctx a
failValidator = Validator . lift . failure

fromValidatinError :: ValidatorContext s ctx -> ValidationError -> GQLError
fromValidatinError
  context@ValidatorContext
    { config
    }
  (ValidationError text locations) =
    GQLError
      { message,
        locations
      }
    where
      message
        | debug config = text <> renderContext context
        | otherwise = text

-- can be only used for internal errors
instance
  (MonadContext (Validator s) s ctx) =>
  Failure InternalError (Validator s ctx)
  where
  failure inputMessage = do
    ctx <- Validator ask
    failure
      ( validationErrorMessage
          (position $ scope ctx)
          $ msg
            inputMessage
            <> renderContext ctx
      )

renderContext :: ValidatorContext s ctx -> Message
renderContext
  ValidatorContext
    { schema,
      scope
    } =
    renderScope scope
      <> renderSection "SchemaDefinition" schema

renderScope :: Scope -> Message
renderScope
  Scope
    { currentTypeName,
      currentTypeKind,
      fieldname
    } =
    renderSection
      "Scope"
      ( "referenced by type "
          <> render currentTypeName
          <> " of kind "
          <> render currentTypeKind
          <> " in field "
          <> render fieldname
      )

renderSection :: RenderGQL a => Message -> a -> Message
renderSection label content =
  "\n\n" <> label <> ":\n" <> line
    <> "\n\n"
    <> msg (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
