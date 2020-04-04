{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.Morpheus.Error.ErrorClass
  ( MissingRequired(..)
  , KindViolation(..)
  , Unknown(..)
  , InternalError(..)
  , Target(..)
  )
  where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Error.Selection  ( unknownSelectionField
                                                , hasNoSubfields
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , RESOLVED
                                                , Ref(..)
                                                , TypeRef(..)
                                                , GQLError(..)
                                                , GQLErrors
                                                , Argument(..)
                                                , ObjectEntry(..)
                                                , Fragment(..)
                                                , Fragments
                                                , ValidationContext(..)
                                                , Variable(..)
                                                , VariableDefinitions
                                                , FieldDefinition(..)
                                                , FieldsDefinition
                                                , InputFieldsDefinition
                                                , Schema
                                                , getOperationName
                                                )


data Target 
  = TARGET_OBJECT 
  | TARGET_INPUT
  | TARGET_UNION

class InternalError a where
  internalError :: a -> GQLError

instance InternalError FieldDefinition where
  internalError FieldDefinition 
    { fieldName
    , fieldType = TypeRef { typeConName } 
    } = GQLError 
      { message 
        = "INTERNAL: Type \"" <> typeConName
        <> "\" referenced by field \"" <> fieldName 
        <> "\" can't found in Schema "
      , locations = []
      }

instance InternalError (f 'TARGET_OBJECT, (Ref, Name)) where
  

class MissingRequired c where 
  missingRequired :: ValidationContext -> Ref -> c -> GQLError

instance MissingRequired (VariableDefinitions s) where
  missingRequired 
    ValidationContext { operationName } 
    Ref { refName , refPosition } _ 
    = GQLError 
      { message 
        = "Variable \"" <> refName
        <> "\" is not defined by operation \""
        <> getOperationName operationName <> "\"."
      , locations = [refPosition]
      }

class Unknown c where
  type UnknownSelector c
  unknown :: ValidationContext -> c -> UnknownSelector c -> GQLErrors

-- {...H} -> "Unknown fragment \"H\"."
instance Unknown Fragments where
  type UnknownSelector Fragments = Ref
  unknown _ _ (Ref name pos) 
    = errorMessage pos
      ("Unknown Fragment \"" <> name <> "\".")

instance Unknown Schema where
  type UnknownSelector Schema = Ref
  unknown _ _ Ref { refName , refPosition }
    = errorMessage refPosition ("Unknown type \"" <> refName <> "\".")

instance Unknown FieldDefinition where
  type UnknownSelector FieldDefinition = Argument RESOLVED
  unknown _ FieldDefinition { fieldName } Argument { argumentName, argumentPosition }
    = errorMessage argumentPosition 
      ("Unknown Argument \"" <> argumentName <> "\" on Field \"" <> fieldName <> "\".")

instance Unknown InputFieldsDefinition where
  type UnknownSelector InputFieldsDefinition = ObjectEntry RESOLVED
  unknown _ _ ObjectEntry { entryName } = 
    [
      GQLError 
        { message = "Unknown Field \"" <> entryName <> "\"."
        , locations = []
        }
    ]

instance Unknown FieldsDefinition where
  type UnknownSelector FieldsDefinition = Ref
  unknown _ _ = unknownSelectionField "TODO: currentTypeName"

class KindViolation (t :: Target) ctx where
  kindViolation :: c t -> ctx -> GQLError

instance KindViolation 'TARGET_OBJECT Fragment where
  kindViolation _ Fragment { fragmentName, fragmentType, fragmentPosition } 
    = GQLError
    { message   
      = "Fragment \"" <> fragmentName 
        <> "\" cannot condition on non composite type \"" 
        <> fragmentType <>"\"."
    , locations = [fragmentPosition]
    }

instance KindViolation 'TARGET_INPUT (Variable s) where
  kindViolation _ Variable 
      { variableName 
      , variablePosition
      , variableType = TypeRef { typeConName }
      } 
    = GQLError 
      { message 
        =  "Variable \"$" <> variableName 
        <> "\" cannot be non-input type \""
        <> typeConName <>"\"." --TODO: render with typewrappers
      , locations = [variablePosition]
      }

instance KindViolation 'TARGET_UNION  (Ref, Name) where
  -- kindViolation (ref,name) = head $ hasNoSubfields ref name

instance KindViolation 'TARGET_OBJECT  (Ref, Name) where
