{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
where

--
-- MORPHEUS
import Control.Applicative (Applicative (..))
import Control.Monad (Monad ((>>=)))
import Data.Functor ((<$>), fmap)
import Data.Map (Map, empty, fromList)
import Data.Maybe (Maybe (..), maybe)
import Data.Morpheus.Internal.TH
  ( apply,
    applyVars,
    toName,
    typeInstanceDec,
  )
import Data.Morpheus.Internal.Utils
  ( elems,
    stripConstructorNamespace,
    stripFieldNamespace,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDecContext (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( funDProxy,
    kindName,
    mkTypeableConstraints,
    tyConArgs,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLTypeOptions (..),
  )
import Data.Morpheus.Types (Resolver, interface)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition,
    DataEnumValue (..),
    Description,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName (..),
    FieldsDefinition,
    IN,
    OUT,
    QUERY,
    TRUE,
    Token,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName (..),
    Value,
  )
import Data.Proxy (Proxy (..))
import Language.Haskell.TH
import Prelude
  ( ($),
    (.),
    concatMap,
    id,
    null,
    otherwise,
  )

interfaceF :: Name -> ExpQ
interfaceF name = [|interface (Proxy :: (Proxy ($(conT name) (Resolver QUERY () Maybe))))|]

introspectInterface :: TypeName -> ExpQ
introspectInterface = interfaceF . toName

dropNamespaceOptions :: TypeKind -> TypeName -> GQLTypeOptions -> GQLTypeOptions
dropNamespaceOptions KindEnum tName opt = opt {constructorTagModifier = stripConstructorNamespace tName}
dropNamespaceOptions _ tName opt = opt {fieldLabelModifier = stripFieldNamespace tName}

deriveGQLType :: ServerDecContext -> ServerTypeDefinition cat s -> Q [Dec]
deriveGQLType
  ServerDecContext {namespace}
  ServerTypeDefinition {tName, tKind, typeOriginal} =
    pure <$> instanceD constrains iHead (typeFamilies : functions)
    where
      functions =
        funDProxy
          [ ('description, [|tDescription|]),
            ('implements, implementsFunc),
            ('typeOptions, typeOptionsFunc),
            ('getDescriptions, fieldDescriptionsFunc),
            ('getDirectives, fieldDirectivesFunc),
            ('getFieldContents, getFieldContentsFunc)
          ]
        where
          tDescription = typeOriginal >>= typeDescription
          implementsFunc = listE $ fmap introspectInterface (interfacesFrom typeOriginal)
          typeOptionsFunc
            | namespace = [|dropNamespaceOptions tKind tName|]
            | otherwise = [|id|]
          fieldDescriptionsFunc = [|value|]
            where
              value = getDesc typeOriginal
          fieldDirectivesFunc = [|value|]
            where
              value = getDirs typeOriginal
          getFieldContentsFunc = [|value|]
            where
              value =
                fmapFieldValues
                  (fmap getDefaultValue . fieldContent)
                  (fmap getDefaultValue . fieldContent)
                  typeOriginal
      --------------------------------
      typeArgs = tyConArgs tKind
      --------------------------------
      iHead = apply ''GQLType [applyVars tName typeArgs]
      headSig = applyVars tName typeArgs
      ---------------------------------------------------
      constrains = mkTypeableConstraints typeArgs
      -------------------------------------------------
      typeFamilies = deriveInstance ''KIND (kindName tKind)
        where
          deriveInstance :: Name -> Name -> Q Dec
          deriveInstance insName tyName = do
            typeN <- headSig
            pure $ typeInstanceDec insName typeN (ConT tyName)

interfacesFrom :: Maybe (TypeDefinition ANY s) -> [TypeName]
interfacesFrom (Just TypeDefinition {typeContent = DataObject {objectImplements}}) = objectImplements
interfacesFrom _ = []

fmapFieldValues :: (FieldDefinition IN s -> Maybe a) -> (FieldDefinition OUT s -> Maybe a) -> Maybe (TypeDefinition c s) -> Map FieldName a
fmapFieldValues f g = maybe empty (collectFieldValues f g)

getDesc :: Maybe (TypeDefinition c s) -> Map Token Description
getDesc = fromList . get

getDirs :: Maybe (TypeDefinition c s) -> Map Token (Directives s)
getDirs = fromList . get

class Meta a v where
  get :: a -> [(Token, v)]

instance (Meta a v) => Meta (Maybe a) v where
  get (Just x) = get x
  get _ = []

instance
  ( Meta (FieldsDefinition IN s) v,
    Meta (FieldsDefinition OUT s) v,
    Meta (DataEnumValue s) v
  ) =>
  Meta (TypeDefinition c s) v
  where
  get TypeDefinition {typeContent} = get typeContent

instance
  ( Meta (FieldsDefinition IN s) v,
    Meta (FieldsDefinition OUT s) v,
    Meta (DataEnumValue s) v
  ) =>
  Meta (TypeContent a c s) v
  where
  get DataObject {objectFields} = get objectFields
  get DataInputObject {inputObjectFields} = get inputObjectFields
  get DataInterface {interfaceFields} = get interfaceFields
  get DataEnum {enumMembers} = concatMap get enumMembers
  get _ = []

instance Meta (DataEnumValue s) Description where
  get DataEnumValue {enumName, enumDescription = Just x} = [(readTypeName enumName, x)]
  get _ = []

instance Meta (DataEnumValue s) (Directives s) where
  get DataEnumValue {enumName, enumDirectives}
    | null enumDirectives = []
    | otherwise = [(readTypeName enumName, enumDirectives)]

instance
  Meta (FieldDefinition c s) v =>
  Meta (FieldsDefinition c s) v
  where
  get = concatMap get . elems

instance Meta (FieldDefinition c s) Description where
  get FieldDefinition {fieldName, fieldDescription = Just x} = [(readName fieldName, x)]
  get _ = []

instance Meta (FieldDefinition c s) (Directives s) where
  get FieldDefinition {fieldName, fieldDirectives}
    | null fieldDirectives = []
    | otherwise = [(readName fieldName, fieldDirectives)]

collectFieldValues ::
  (FieldDefinition IN s -> Maybe a) ->
  (FieldDefinition OUT s -> Maybe a) ->
  TypeDefinition c s ->
  Map FieldName a
collectFieldValues _ g TypeDefinition {typeContent = DataObject {objectFields}} = getFieldValues g objectFields
collectFieldValues f _ TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = getFieldValues f inputObjectFields
collectFieldValues _ g TypeDefinition {typeContent = DataInterface {interfaceFields}} = getFieldValues g interfaceFields
collectFieldValues _ _ _ = empty

getFieldValues :: (FieldDefinition c s -> Maybe a) -> FieldsDefinition c s -> Map FieldName a
getFieldValues f = fromList . notNulls . fmap (getFieldValue f) . elems

notNulls :: [(k, Maybe a)] -> [(k, a)]
notNulls [] = []
notNulls ((_, Nothing) : xs) = notNulls xs
notNulls ((name, Just x) : xs) = (name, x) : notNulls xs

getFieldValue :: (FieldDefinition c s -> Maybe a) -> FieldDefinition c s -> (FieldName, Maybe a)
getFieldValue f field = (fieldName field, f field)

getDefaultValue :: FieldContent TRUE c s -> (Maybe (Value s), Maybe (ArgumentsDefinition s))
getDefaultValue DefaultInputValue {defaultInputValue} = (Just defaultInputValue, Nothing)
getDefaultValue (FieldArgs args) = (Nothing, Just args)
