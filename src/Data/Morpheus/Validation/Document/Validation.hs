{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}


module Data.Morpheus.Validation.Document.Validation
  ( validatePartialDocument
  )
where

import           Data.Maybe

--
-- Morpheus
import           Data.Morpheus.Error.Document.Interface
                                                ( ImplementsError(..)
                                                , partialImplements
                                                , unknownInterface
                                                )
import           Data.Morpheus.Rendering.RenderGQL
                                                ( RenderGQL(..) )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , DataField(..)
                                                , DataType(..)
                                                , FieldsDefinition(..)
                                                , DataTypeContent(..)
                                                , TypeRef(..)
                                                , isWeaker
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )

validatePartialDocument :: [(Name, DataType)] -> Validation [(Name, DataType)]
validatePartialDocument lib = catMaybes <$> traverse validateType lib
 where
  validateType :: (Name, DataType) -> Validation (Maybe (Name, DataType))
  validateType (name, dt@DataType { typeName , typeContent = DataObject { objectImplements , objectFields}  }) = do         
      interface <- traverse getInterfaceByKey objectImplements
      case concatMap (mustBeSubset objectFields) interface of
        [] -> pure $ Just (name, dt) 
        errors -> failure $ partialImplements typeName errors
  validateType (_,DataType { typeContent = DataInterface {}}) = pure Nothing
  validateType (name, x) = pure $ Just (name, x)
  mustBeSubset
    :: FieldsDefinition -> (Name, FieldsDefinition) -> [(Name, Name, ImplementsError)]
  mustBeSubset objFields (typeName, FieldsDefinition fields) = concatMap checkField fields
   where
    checkField :: (Name, DataField) -> [(Name, Name, ImplementsError)]
    checkField (key, DataField { fieldType = interfaceT@TypeRef { typeConName = interfaceTypeName, typeWrappers = interfaceWrappers } })
      = case lookup key (unFieldsDefinition objFields) of
        Just DataField { fieldType = objT@TypeRef { typeConName, typeWrappers } }
          | typeConName == interfaceTypeName && not
            (isWeaker typeWrappers interfaceWrappers)
          -> []
          | otherwise
          -> [ ( typeName
               , key
               , UnexpectedType { expectedType = render interfaceT
                                , foundType    = render objT
                                }
               )
             ]
        Nothing -> [(typeName, key, UndefinedField)]
  -------------------------------
  getInterfaceByKey :: Name -> Validation (Name, FieldsDefinition)
  getInterfaceByKey key = case lookup key lib of
    Just DataType { typeContent = DataInterface { interfaceFields } } -> pure (key,interfaceFields)
    _ -> failure $ unknownInterface key
