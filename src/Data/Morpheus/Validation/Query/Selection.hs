{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateOperation
  )
where


-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( hasNoSubfields
                                                , subfieldsNotSelected
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( VariableDefinitions
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , Fragment(..)
                                                , SelectionSet
                                                , FieldDefinition(..)
                                                , FieldsDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
                                                , Operation(..)
                                                , Ref(..)
                                                , Name
                                                , RAW
                                                , VALID
                                                , Arguments
                                                , isEntNode
                                                )
import           Data.Morpheus.Types.Internal.AST.MergeSet
                                                ( concatTraverse )
import           Data.Morpheus.Types.Internal.Operation
                                                ( empty
                                                , singleton
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( SelectionValidator
                                                , askFieldType
                                                , selectKnown
                                                , withScope
                                                )
import           Data.Morpheus.Validation.Query.UnionSelection
                                                (validateUnionSelection)
import           Data.Morpheus.Validation.Query.Arguments
                                                ( validateArguments )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( castFragmentType
                                                , resolveSpread
                                                )

type TypeDef = (Name, FieldsDefinition)

validateOperation
  :: TypeDef
  -> Operation RAW
  -> SelectionValidator (SelectionSet VALID)
validateOperation tyDef Operation { operationSelection } = 
    __validate 
      tyDef 
      operationSelection
 where
  __validate
    :: TypeDef -> SelectionSet RAW -> SelectionValidator (SelectionSet VALID)
  __validate dataType@(typeName,fieldsDef) =
      concatTraverse validateSelection 
   where
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    validateSelection :: Selection RAW -> SelectionValidator (SelectionSet VALID)
    validateSelection 
        sel@Selection 
          { selectionName
          , selectionArguments
          , selectionContent
          , selectionPosition 
          } 
      = withScope 
        typeName 
        selectionPosition $ 
        validateSelectionContent selectionContent
      where
        commonValidation :: SelectionValidator (TypeDefinition, Arguments VALID)
        commonValidation  = do
          (fieldDef :: FieldDefinition) <- selectKnown (Ref selectionName selectionPosition) fieldsDef
          -- validate field Argument -----
          arguments <- validateArguments
                        fieldDef
                        selectionArguments
          -- check field Type existence  -----
          (typeDef :: TypeDefinition) <- askFieldType fieldDef
          pure (typeDef, arguments)
        -----------------------------------------------------------------------------------
        validateSelectionContent :: SelectionContent RAW -> SelectionValidator (SelectionSet VALID)
        validateSelectionContent SelectionField 
            | null selectionArguments && selectionName == "__typename" 
              = pure $ singleton $ sel { selectionArguments = empty, selectionContent = SelectionField }
            | otherwise = do
              (datatype, validArgs) <- commonValidation
              isLeaf datatype
              pure $ singleton $ sel { selectionArguments = validArgs, selectionContent = SelectionField }
         where
          ------------------------------------------------------------
          isLeaf :: TypeDefinition -> SelectionValidator ()
          isLeaf TypeDefinition { typeName = typename, typeContent }
              | isEntNode typeContent = pure ()
              | otherwise = failure
              $ subfieldsNotSelected selectionName typename selectionPosition
        ----- SelectionSet
        validateSelectionContent (SelectionSet rawSelectionSet)
          = do
            (TypeDefinition { typeName = name , typeContent}, validArgs) <- commonValidation
            selContent <- withScope name selectionPosition $ validateByTypeContent name typeContent
            pure $ singleton $ sel { selectionArguments = validArgs, selectionContent = selContent }
           where
            validateByTypeContent :: Name -> TypeContent -> SelectionValidator (SelectionContent VALID)
            -- Validate UnionSelection  
            validateByTypeContent _ DataUnion { unionMembers } 
              = validateUnionSelection  
                    __validate
                    rawSelectionSet 
                    unionMembers
            -- Validate Regular selection set
            validateByTypeContent typename DataObject { objectFields } 
              = SelectionSet 
                  <$> __validate 
                        (typename, objectFields) 
                        rawSelectionSet
            validateByTypeContent typename _ 
              = failure 
                  $ hasNoSubfields 
                      (Ref selectionName selectionPosition) 
                      typename
    validateSelection (Spread ref) 
      = resolveSpread [typeName] ref 
        >>= validateFragment
    validateSelection (InlineFragment fragment') 
      = castFragmentType Nothing (fragmentPosition fragment') [typeName] fragment'
        >>= validateFragment
    --------------------------------------------------------------------------------
    validateFragment Fragment { fragmentSelection } = __validate dataType fragmentSelection
