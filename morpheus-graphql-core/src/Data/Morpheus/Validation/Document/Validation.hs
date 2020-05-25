{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
    validateSchema,
  )
where

import Data.Functor (($>))
--
-- Morpheus

import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    partialImplements,
    unknownInterface,
  )
import Data.Morpheus.Error.Utils (globalErrorMessage)
import Data.Morpheus.Internal.Utils
  ( Selectable (..),
    elems,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    OUT,
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    isWeaker,
    msg,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Failure (..),
  )

type CTX = [TypeDefinition ANY]

validateSchema :: Schema -> Eventless Schema
validateSchema schema = validatePartialDocument (elems schema) $> schema

validatePartialDocument :: [TypeDefinition ANY] -> Eventless [TypeDefinition ANY]
validatePartialDocument schema = traverse (validateType schema) schema

validateType ::
  CTX ->
  TypeDefinition ANY ->
  Eventless (TypeDefinition ANY)
validateType
  ctx
  dt@TypeDefinition
    { typeName,
      typeContent = DataObject {objectImplements, objectFields}
    } = do
    validateImplements ctx typeName objectImplements objectFields
    pure dt
validateType _ x = pure x

-- INETRFACE
----------------------------
validateImplements ::
  CTX ->
  TypeName ->
  [TypeName] ->
  FieldsDefinition OUT ->
  Eventless ()
validateImplements ctx typeName objectImplements objectFields = do
  interface <- traverse (getInterfaceByKey ctx) objectImplements
  case concatMap (mustBeSubset objectFields) interface of
    [] -> pure ()
    errors -> failure $ partialImplements typeName errors

mustBeSubset ::
  FieldsDefinition OUT -> (TypeName, FieldsDefinition OUT) -> [(TypeName, FieldName, ImplementsError)]
mustBeSubset objFields (typeName, fields) = concatMap (checkInterfaceField typeName objFields) (elems fields)

checkInterfaceField ::
  TypeName ->
  FieldsDefinition OUT ->
  FieldDefinition OUT ->
  [(TypeName, FieldName, ImplementsError)]
checkInterfaceField
  typeName
  objFields
  FieldDefinition
    { fieldName,
      fieldType =
        interfaceT@TypeRef
          { typeConName = interfaceTypeName,
            typeWrappers = interfaceWrappers
          }
    } =
    selectOr err checkTypeEq fieldName objFields
    where
      err = [(typeName, fieldName, UndefinedField)]
      checkTypeEq FieldDefinition {fieldType = objT@TypeRef {typeConName, typeWrappers}}
        | typeConName == interfaceTypeName && not (isWeaker typeWrappers interfaceWrappers) =
          []
        | otherwise =
          [ ( typeName,
              fieldName,
              UnexpectedType
                { expectedType = interfaceT,
                  foundType = objT
                }
            )
          ]

-------------------------------
getInterfaceByKey :: CTX -> TypeName -> Eventless (TypeName, FieldsDefinition OUT)
getInterfaceByKey schema interfaceName =
  selectBy err interfaceName schema
    >>= constraintInterface
  where
    err = unknownInterface interfaceName
    constraintInterface TypeDefinition {typeContent = DataInterface {interfaceFields}} = pure (interfaceName, interfaceFields)
    constraintInterface _ = failure $ globalErrorMessage $ "type " <> msg interfaceName <> " must be an interface"
