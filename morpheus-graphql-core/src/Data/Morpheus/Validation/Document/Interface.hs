{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Validation.Document.Interface
  ( validateImplements,
  )
where

import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    partialImplements,
  )
import Data.Morpheus.Internal.Utils
  ( KeyOf (..),
    Selectable (..),
    empty,
    failure,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentDefinition (..),
    ArgumentsDefinition,
    CONST,
    FieldContent (..),
    FieldDefinition (..),
    FieldsDefinition,
    OUT,
    Subtyping (..),
    TRUE,
    TypeName,
    TypeRef (..),
  )
import Data.Morpheus.Types.Internal.Validation (selectType)
import Data.Morpheus.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    ON_INTERFACE,
    ON_TYPE,
    PLACE,
    SchemaValidator,
    TypeEntity (..),
    TypeSystemContext (..),
    constraintInterface,
    inArgument,
    inField,
    inInterface,
  )
import Relude hiding (empty, local)

validateImplements ::
  [TypeName] ->
  FieldsDefinition OUT CONST ->
  SchemaValidator (TypeEntity ON_TYPE) [TypeName]
validateImplements interfaceNames objectFields =
  traverse (selectType >=> constraintInterface >=> hasCompatibleFields) interfaceNames
    $> interfaceNames
  where
    hasCompatibleFields :: (TypeName, FieldsDefinition OUT CONST) -> SchemaValidator (TypeEntity ON_TYPE) ()
    hasCompatibleFields (typeName, fields) = inInterface typeName $ isCompatibleTo objectFields fields

class StructuralCompatibility a where
  type Context a :: PLACE -> *
  type Context a = Field

  -- Object (which implements interface) -> Interface -> Validation
  isCompatibleTo :: a -> a -> SchemaValidator ((Context a) ON_INTERFACE) ()

  isCompatibleBy :: (t -> a) -> t -> t -> SchemaValidator ((Context a) ON_INTERFACE) ()
  isCompatibleBy f a b = f a `isCompatibleTo` f b

instance StructuralCompatibility (FieldsDefinition OUT s) where
  type Context (FieldsDefinition OUT s) = TypeEntity
  isCompatibleTo objFields = traverse_ checkInterfaceField
    where
      checkInterfaceField interfaceField@FieldDefinition {fieldName} =
        inField fieldName $ selectOr err (`isCompatibleTo` interfaceField) fieldName objFields
        where
          err = failImplements Missing

instance StructuralCompatibility (FieldDefinition OUT s) where
  f1 `isCompatibleTo` f2 =
    isCompatibleBy fieldType f1 f2
      *> isCompatibleBy (fieldArgs . fieldContent) f1 f2

fieldArgs :: Maybe (FieldContent TRUE OUT s) -> ArgumentsDefinition s
fieldArgs (Just (FieldArgs args)) = args
fieldArgs _ = empty

instance StructuralCompatibility (ArgumentsDefinition s) where
  subArguments `isCompatibleTo` arguments = traverse_ hasCompatibleSubArgument arguments
    where
      hasCompatibleSubArgument argument =
        inArgument (keyOf argument) $
          selectOr (failImplements Missing) (`isCompatibleTo` argument) (keyOf argument) subArguments

instance StructuralCompatibility (ArgumentDefinition s) where
  isCompatibleTo = isCompatibleBy (fieldType . argument)

instance StructuralCompatibility TypeRef where
  t1 `isCompatibleTo` t2
    | t1 `isSubtype` t2 = pure ()
    | otherwise = failImplements UnexpectedType {expectedType = t2, foundType = t1}

failImplements ::
  ImplementsError ->
  SchemaValidator (Field ON_INTERFACE) a
failImplements err = do
  x <- asks local
  failure $ partialImplements x err
