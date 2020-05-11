{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument,
    validateSchema,
  )
where

import Data.Functor (($>))
import Data.Maybe
--
-- Morpheus
import Data.Morpheus.Error.Document.Interface
  ( ImplementsError (..),
    partialImplements,
    unknownInterface,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition (..),
    FieldsDefinition (..),
    Name,
    Schema,
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    isWeaker,
    lookupWith,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Listable (..),
    Selectable (..),
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Failure (..),
  )

validateSchema :: Schema -> Eventless Schema
validateSchema schema = validatePartialDocument (toList schema) $> schema

validatePartialDocument :: [TypeDefinition ANY] -> Eventless [TypeDefinition ANY]
validatePartialDocument lib = catMaybes <$> traverse validateType lib
  where
    validateType :: TypeDefinition ANY -> Eventless (Maybe (TypeDefinition ANY))
    validateType dt@TypeDefinition {typeName, typeContent = DataObject {objectImplements, objectFields}} = do
      interface <- traverse getInterfaceByKey objectImplements
      case concatMap (mustBeSubset objectFields) interface of
        [] -> pure (Just dt)
        errors -> failure $ partialImplements typeName errors
    validateType x = pure (Just x)
    mustBeSubset ::
      FieldsDefinition -> (Name, FieldsDefinition) -> [(Name, Name, ImplementsError)]
    mustBeSubset objFields (typeName, fields) = concatMap checkField (toList fields)
      where
        checkField :: FieldDefinition -> [(Name, Name, ImplementsError)]
        checkField FieldDefinition {fieldName, fieldType = interfaceT@TypeRef {typeConName = interfaceTypeName, typeWrappers = interfaceWrappers}} =
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
                      { expectedType = render interfaceT,
                        foundType = render objT
                      }
                  )
                ]
    -------------------------------
    getInterfaceByKey :: Name -> Eventless (Name, FieldsDefinition)
    getInterfaceByKey interfaceName = case lookupWith typeName interfaceName lib of
      Just TypeDefinition {typeContent = DataInterface {interfaceFields}} -> pure (interfaceName, interfaceFields)
      _ -> failure $ unknownInterface interfaceName
