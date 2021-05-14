{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Fields
  ( Arguments,
    Argument (..),
    ArgumentDefinition (..),
    ArgumentsDefinition,
    FieldDefinition (..),
    FieldsDefinition,
    FieldContent (..),
    InputFieldsDefinition,
    DirectiveDefinitions,
    DirectiveDefinition (..),
    Directives,
    Directive (..),
    fieldVisibility,
    lookupDeprecated,
    lookupDeprecatedReason,
    unsafeFromFields,
    fieldsToArguments,
    fieldContentArgs,
    mkObjectField,
    mkField,
    renderArgumentValues,
    renderDirectives,
  )
where

import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Ext.OrdMap
  ( OrdMap,
    unsafeFromList,
  )
import Data.Morpheus.Internal.Utils
  ( Elems (..),
    Empty (..),
    KeyOf (..),
    Selectable (..),
    toPair,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    intercalate,
    renderArguments,
    renderEntry,
    renderObject,
    space,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    FieldName,
    FieldName (..),
    Msg (..),
    Position,
    TRUE,
    TypeName,
    ValidationError (..),
    msgValidation,
    sysFields,
  )
import Data.Morpheus.Types.Internal.AST.DirectiveLocation (DirectiveLocation)
import Data.Morpheus.Types.Internal.AST.Stage
  ( Stage,
  )
import Data.Morpheus.Types.Internal.AST.Type
  ( Nullable (..),
    TypeRef (..),
    TypeWrapper (..),
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( ANY,
    IN,
    OUT,
    ToCategory (..),
    TypeCategory,
    toAny,
    type (<=?),
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( ScalarValue (..),
    Value (..),
  )
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding (empty, intercalate)

-- scalar
------------------------------------------------------------------
data Argument (valid :: Stage) = Argument
  { argumentPosition :: Position,
    argumentName :: FieldName,
    argumentValue :: Value valid
  }
  deriving (Show, Eq, Lift)

instance KeyOf FieldName (Argument stage) where
  keyOf = argumentName

instance RenderGQL (Argument s) where
  renderGQL Argument {argumentName, argumentValue} =
    renderEntry argumentName argumentValue

instance NameCollision (Argument s) where
  nameCollision Argument {argumentName, argumentPosition} =
    ValidationError
      { validationMessage = "There can Be only One Argument Named " <> msg argumentName,
        validationLocations = [argumentPosition]
      }

type Arguments (s :: Stage) = OrdMap FieldName (Argument s)

renderArgumentValues :: Arguments s -> Rendering
renderArgumentValues = renderArguments . filter notNull . elems
  where
    notNull Argument {argumentValue = Null} = False
    notNull _ = True

-- directive
------------------------------------------------------------------
data Directive (s :: Stage) = Directive
  { directivePosition :: Position,
    directiveName :: FieldName,
    directiveArgs :: Arguments s
  }
  deriving (Show, Lift, Eq)

instance KeyOf FieldName (Directive s) where
  keyOf = directiveName

instance RenderGQL (Directive s) where
  renderGQL Directive {..} =
    "@" <> renderGQL directiveName
      <> renderArgumentValues directiveArgs

type Directives s = [Directive s]

renderDirectives :: Directives s -> Rendering
renderDirectives xs
  | null dirs = ""
  | otherwise = space <> intercalate space (fmap renderGQL dirs)
  where
    dirs = filter notSystem xs
    notSystem Directive {directiveName = "include"} = False
    notSystem Directive {directiveName = "skip"} = False
    notSystem _ = True

data DirectiveDefinition s = DirectiveDefinition
  { directiveDefinitionName :: FieldName,
    directiveDefinitionDescription :: Maybe Description,
    directiveDefinitionArgs :: ArgumentsDefinition s,
    directiveDefinitionLocations :: [DirectiveLocation]
  }
  deriving (Show, Lift)

type DirectiveDefinitions s = [DirectiveDefinition s]

instance KeyOf FieldName (DirectiveDefinition s) where
  keyOf = directiveDefinitionName

instance Selectable FieldName (ArgumentDefinition s) (DirectiveDefinition s) where
  selectOr fb f key DirectiveDefinition {directiveDefinitionArgs} =
    selectOr fb f key directiveDefinitionArgs

lookupDeprecated :: [Directive s] -> Maybe (Directive s)
lookupDeprecated = find isDeprecation
  where
    isDeprecation Directive {directiveName = "deprecated"} = True
    isDeprecation _ = False

lookupDeprecatedReason :: Directive s -> Maybe Description
lookupDeprecatedReason Directive {directiveArgs} =
  selectOr
    Nothing
    argumentStringValue
    ("reason" :: FieldName)
    directiveArgs

argumentStringValue :: Argument s -> Maybe Description
argumentStringValue Argument {argumentValue = Null} = Nothing
argumentStringValue Argument {argumentValue = (Scalar (String x))} = Just x
argumentStringValue _ = Just "can't read deprecated Reason Value"

instance ToCategory FieldDefinition a ANY where
  toCategory FieldDefinition {fieldContent, ..} = FieldDefinition {fieldContent = toAny <$> fieldContent, ..}

instance ToCategory (FieldContent TRUE) a ANY where
  toCategory (FieldArgs x) = FieldArgs x
  toCategory (DefaultInputValue x) = DefaultInputValue x

unsafeFromFields :: [FieldDefinition cat s] -> FieldsDefinition cat s
unsafeFromFields = unsafeFromList . fmap toPair

fieldsToArguments :: FieldsDefinition IN s -> ArgumentsDefinition s
fieldsToArguments = fmap ArgumentDefinition

-- 3.6 Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Objects
------------------------------------------------------------------------------
--  ObjectTypeDefinition:
--    Description(opt) type Name ImplementsInterfaces(opt) Directives(Const)(opt) FieldsDefinition(opt)
--
--  ImplementsInterfaces
--    implements &(opt) NamedType
--    ImplementsInterfaces & NamedType
--
--  FieldsDefinition
--    { FieldDefinition(list) }
--
type FieldsDefinition cat s = OrdMap FieldName (FieldDefinition cat s)

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
-- InputValueDefinition
--   Description(opt) Name: Type DefaultValue(opt) Directives[Const](opt)

data FieldDefinition (cat :: TypeCategory) (s :: Stage) = FieldDefinition
  { fieldDescription :: Maybe Description,
    fieldName :: FieldName,
    fieldType :: TypeRef,
    fieldContent :: Maybe (FieldContent TRUE cat s),
    fieldDirectives :: [Directive s]
  }
  deriving (Show, Lift, Eq)

data FieldContent (bool :: Bool) (cat :: TypeCategory) (s :: Stage) where
  DefaultInputValue ::
    { defaultInputValue :: Value s
    } ->
    FieldContent (IN <=? cat) cat s
  FieldArgs ::
    { fieldArgsDef :: ArgumentsDefinition s
    } ->
    FieldContent (OUT <=? cat) cat s

fieldContentArgs :: FieldContent b cat s -> ArgumentsDefinition s
fieldContentArgs (FieldArgs args) = args
fieldContentArgs _ = empty

deriving instance Eq (FieldContent bool cat s)

deriving instance Show (FieldContent bool cat s)

deriving instance Lift (FieldContent bool cat s)

instance KeyOf FieldName (FieldDefinition cat s) where
  keyOf = fieldName

instance Selectable FieldName (ArgumentDefinition s) (FieldDefinition OUT s) where
  selectOr fb f key FieldDefinition {fieldContent = Just (FieldArgs args)} = selectOr fb f key args
  selectOr fb _ _ _ = fb

instance NameCollision (FieldDefinition cat s) where
  nameCollision FieldDefinition {fieldName} =
    "There can Be only One field Named " <> msgValidation fieldName

instance RenderGQL (FieldDefinition cat s) where
  renderGQL FieldDefinition {fieldName, fieldType, fieldContent = Just (FieldArgs args)} =
    renderGQL fieldName <> renderGQL args <> ": " <> renderGQL fieldType
  renderGQL FieldDefinition {fieldName, fieldType} =
    renderEntry fieldName fieldType

instance RenderGQL (FieldsDefinition cat s) where
  renderGQL = renderObject . filter fieldVisibility . elems

instance Nullable (FieldDefinition cat s) where
  isNullable = isNullable . fieldType
  toNullable field = field {fieldType = toNullable (fieldType field)}

fieldVisibility :: FieldDefinition cat s -> Bool
fieldVisibility FieldDefinition {fieldName} = fieldName `notElem` sysFields

mkField ::
  Maybe (FieldContent TRUE cat s) ->
  FieldName ->
  TypeRef ->
  FieldDefinition cat s
mkField fieldContent fieldName fieldType =
  FieldDefinition
    { fieldName,
      fieldContent,
      fieldDescription = Nothing,
      fieldType,
      fieldDirectives = []
    }

mkObjectField ::
  ArgumentsDefinition s ->
  FieldName ->
  TypeWrapper ->
  TypeName ->
  FieldDefinition OUT s
mkObjectField args fieldName typeWrappers typeConName =
  mkField
    (Just $ FieldArgs args)
    fieldName
    TypeRef {typeWrappers, typeConName}

-- 3.10 Input Objects: https://spec.graphql.org/June2018/#sec-Input-Objects
---------------------------------------------------------------------------
--- InputFieldsDefinition
-- { InputValueDefinition(list) }

type InputFieldsDefinition s = OrdMap FieldName (InputValueDefinition s)

type InputValueDefinition = FieldDefinition IN

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

type ArgumentsDefinition s = OrdMap FieldName (ArgumentDefinition s)

instance RenderGQL (ArgumentsDefinition s) where
  renderGQL = renderArguments . elems

instance RenderGQL (ArgumentDefinition s) where
  renderGQL = renderGQL . argument

newtype ArgumentDefinition s = ArgumentDefinition
  { argument :: FieldDefinition IN s
  }
  deriving (Show, Lift, Eq)

instance KeyOf FieldName (ArgumentDefinition s) where
  keyOf = keyOf . argument

instance NameCollision (ArgumentDefinition s) where
  nameCollision ArgumentDefinition {argument} =
    "There can Be only One argument Named " <> msgValidation (fieldName argument)
