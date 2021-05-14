{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Document.Interface
  ( unknownInterface,
    ImplementsError (..),
    partialImplements,
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( TypeName (..),
    ValidationError,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.AST.Type (TypeRef)
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    InterfaceName (..),
    ON_INTERFACE,
    TypeEntity (..),
    renderField,
  )
import Relude

unknownInterface :: TypeName -> ValidationError
unknownInterface name = "Unknown Interface " <> msgValidation name <> "."

data ImplementsError
  = UnexpectedType
      { expectedType :: TypeRef,
        foundType :: TypeRef
      }
  | Missing

partialImplements :: Field ON_INTERFACE -> ImplementsError -> ValidationError
partialImplements (Field fieldName argName (TypeEntity (OnInterface interfaceName) typename)) errorType =
  "Interface field " <> maybe "" (const "argument ") argName
    <> renderField interfaceName fieldName argName
    <> detailedMessageGen
      (renderField typename fieldName argName)
      (maybe (msgValidation typename) (const $ renderField typename fieldName Nothing) argName)
      errorType

-- Interface field TestInterface.name expected but User does not provide it.
-- Interface field TestInterface.name expects type String! but User.name is type Int!.
-- Interface field argument TestInterface.name(id:) expected but User.name does not provide it.
-- Interface field argument TestInterface.name(id:) expects type ID but User.name(id:) is type String.

detailedMessageGen :: ValidationError -> ValidationError -> ImplementsError -> ValidationError
detailedMessageGen pl1 _ UnexpectedType {expectedType, foundType} =
  " expects type "
    <> msgValidation expectedType
    <> " but "
    <> pl1
    <> " is type "
    <> msgValidation foundType
    <> "."
detailedMessageGen _ pl2 Missing = " expected but " <> pl2 <> " does not provide it."
