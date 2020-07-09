{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Directive
  ( shouldIncludeSelection,
    validateDirectives,
    Validate,
  )
where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.List (elem)
import Data.Morpheus.Error (errorMessage, globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    selectBy,
    selectOr,
  )
import Data.Morpheus.Schema.Directives (defaultDirectives)
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    CONST,
    Directive (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),
    Directives,
    FieldName,
    RAW,
    ScalarValue (..),
    VALID,
    Value (..),
    msg,
  )
import Data.Morpheus.Types.Internal.Validation
  ( Validate (..),
    Validator,
    selectKnown,
    withDirective,
  )
import qualified Data.Morpheus.Validation.Internal.Arguments as A
  ( ArgCTX,
    Validate,
    validateDirectiveArguments,
  )
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Prelude
  ( ($),
    (&&),
    (==),
    Bool (..),
    otherwise,
  )

validateDirectives ::
  Validate DirectiveLocation Directive s ctx =>
  DirectiveLocation ->
  Directives s ->
  Validator ctx (Directives VALID)
validateDirectives location = traverse (validate location)

-- class ValidateDirective (s :: Stage) ctx where
--   validateDirective :: DirectiveLocation -> Directive s -> Validator ctx (Directive VALID)

instance
  ( A.Validate (A.ArgCTX ctx VALID) RAW ctx
  ) =>
  Validate DirectiveLocation Directive RAW ctx
  where
  validate location directive@Directive {directiveArgs, ..} =
    withDirective directive $ do
      (directiveDef :: DirectiveDefinition VALID) <- selectKnown directive defaultDirectives
      args <- A.validateDirectiveArguments directiveDef directiveArgs
      validateDirectiveLocation location directive directiveDef
      pure Directive {directiveArgs = args, ..}

instance
  ( A.Validate (A.ArgCTX ctx CONST) CONST ctx
  ) =>
  Validate DirectiveLocation Directive CONST ctx
  where
  validate location directive@Directive {directiveArgs = args, ..} =
    withDirective directive $ do
      (directiveDef :: DirectiveDefinition CONST) <- selectKnown directive defaultDirectives
      directiveArgs <- A.validateDirectiveArguments directiveDef args
      validateDirectiveLocation location directive directiveDef
      pure Directive {..}

validateDirectiveLocation ::
  DirectiveLocation ->
  Directive s ->
  DirectiveDefinition s' ->
  Validator ctx ()
validateDirectiveLocation
  loc
  Directive {directiveName, directivePosition}
  DirectiveDefinition {directiveDefinitionLocations}
    | loc `elem` directiveDefinitionLocations = pure ()
    | otherwise =
      failure $
        errorMessage
          directivePosition
          ("Directive " <> msg directiveName <> " may not to be used on " <> msg loc)

directiveFulfilled ::
  Bool ->
  FieldName ->
  Directives s ->
  Validator ctx Bool
directiveFulfilled target = selectOr (pure True) (argumentIf target)

shouldIncludeSelection ::
  Directives VALID ->
  Validator ctx Bool
shouldIncludeSelection directives = do
  dontSkip <- directiveFulfilled False "skip" directives
  include <- directiveFulfilled True "include" directives
  pure (dontSkip && include)

argumentIf ::
  Bool ->
  Directive s ->
  Validator ctx Bool
argumentIf target Directive {directiveName, directiveArgs} =
  selectBy err "if" directiveArgs
    >>= assertArgument target
  where
    err = globalErrorMessage $ "Directive " <> msg ("@" <> directiveName) <> " argument \"if\" of type \"Boolean!\" is required but not provided."

assertArgument ::
  Bool ->
  Argument s ->
  Validator ctx Bool
assertArgument asserted Argument {argumentValue = Scalar (Boolean actual)} = pure (asserted == actual)
assertArgument _ Argument {argumentValue} = failure $ "Expected type Boolean!, found " <> msg argumentValue <> "."
