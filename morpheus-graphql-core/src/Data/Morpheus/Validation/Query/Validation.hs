{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.Validation
  ( validateRequest,
  )
where

import Data.Morpheus.Ext.Result
  ( Eventless,
  )
import Data.Morpheus.Internal.Utils (empty)
import Data.Morpheus.Types.Internal.AST
  ( ExecutableDocument (..),
    Operation (..),
    Schema (..),
    TypeKind (..),
    VALID,
    mkBaseType,
  )
import Data.Morpheus.Types.Internal.Config (Config (..))
import Data.Morpheus.Types.Internal.Validation
  ( CurrentSelection (..),
    OperationContext (..),
    Scope (..),
    ScopeKind (..),
    runValidator,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( validateFragments,
  )
import Data.Morpheus.Validation.Query.FragmentPreconditions
  ( checkFragmentPreconditions,
  )
import Data.Morpheus.Validation.Query.Selection
  ( validateFragmentSelection,
    validateOperation,
  )
import Data.Morpheus.Validation.Query.Variable
  ( resolveOperationVariables,
  )
import Relude hiding
  ( empty,
    fromList,
  )

validateRequest ::
  Config ->
  Schema VALID ->
  ExecutableDocument ->
  Eventless (Operation VALID)
validateRequest
  config
  schema
  ExecutableDocument
    { fragments,
      inputVariables,
      operation =
        operation@Operation
          { operationName,
            operationSelection,
            operationPosition
          }
    } =
    do
      variables <-
        runValidator
          validateHelpers
          config
          schema
          scope
          ( OperationContext
              { selection,
                fragments,
                variables = empty
              }
          )
      validFragments <-
        runValidator
          (validateFragments validateFragmentSelection)
          config
          schema
          scope
          ( OperationContext
              { selection,
                fragments,
                variables
              }
          )
      runValidator
        (validateOperation operation)
        config
        schema
        scope
        ( OperationContext
            { selection,
              fragments = validFragments,
              variables
            }
        )
    where
      scope =
        Scope
          { kind = SELECTION,
            currentTypeName = "Root",
            currentTypeKind = KindObject Nothing,
            currentTypeWrappers = mkBaseType,
            fieldname = "Root",
            position = Just operationPosition
          }
      selection = CurrentSelection {operationName}
      validateHelpers =
        checkFragmentPreconditions operationSelection
          *> resolveOperationVariables
            config
            inputVariables
            operation
