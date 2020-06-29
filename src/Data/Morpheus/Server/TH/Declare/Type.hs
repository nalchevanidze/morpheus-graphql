{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
where

import Data.Morpheus.Internal.TH
  ( declareTypeRef,
    m',
    mkFieldName,
    mkTypeName,
    nameSpaceField,
    nameSpaceType,
    tyConArgs,
  )
import Data.Morpheus.Server.Internal.TH.Types (ServerTypeDefinition (..))
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    OUT,
    TRUE,
    TypeKind (..),
    TypeName,
    isOutput,
    isOutputObject,
    isSubscription,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( SubscriptionField,
  )
import GHC.Generics (Generic)
import Language.Haskell.TH

declareType :: Bool -> ServerTypeDefinition cat -> [Dec]
declareType _ ServerTypeDefinition {tKind = KindScalar} = []
declareType namespace ServerTypeDefinition {tName, tCons, tKind, tNamespace} =
  [ DataD
      []
      (mkNamespace tNamespace tName)
      tVars
      Nothing
      cons
      (derive tKind)
  ]
  where
    tVars = declareTyVar (tyConArgs tKind)
      where
        declareTyVar = map (PlainTV . mkTypeName)
    cons = declareCons namespace tKind (tNamespace, tName) tCons

derive :: TypeKind -> [DerivClause]
derive tKind = [deriveClasses (''Generic : derivingList)]
  where
    derivingList
      | isOutput tKind = []
      | otherwise = [''Show]

deriveClasses :: [Name] -> DerivClause
deriveClasses classNames = DerivClause Nothing (map ConT classNames)

mkNamespace :: [FieldName] -> TypeName -> Name
mkNamespace tNamespace = mkTypeName . nameSpaceType tNamespace

declareCons ::
  Bool ->
  TypeKind ->
  ([FieldName], TypeName) ->
  [ConsD cat] ->
  [Con]
declareCons namespace tKind (tNamespace, tName) = map consR
  where
    consR ConsD {cName, cFields} =
      RecC
        (mkNamespace tNamespace cName)
        (map (declareField namespace tKind tName) cFields)

declareField ::
  Bool ->
  TypeKind ->
  TypeName ->
  FieldDefinition cat ->
  (Name, Bang, Type)
declareField namespace tKind tName field@FieldDefinition {fieldName} =
  ( fieldTypeName namespace tName fieldName,
    Bang NoSourceUnpackedness NoSourceStrictness,
    renderFieldType tKind field
  )

renderFieldType ::
  TypeKind ->
  FieldDefinition cat ->
  Type
renderFieldType tKind FieldDefinition {fieldContent, fieldType} =
  withFieldWrappers tKind fieldContent (declareTypeRef fieldType)

fieldTypeName :: Bool -> TypeName -> FieldName -> Name
fieldTypeName namespace tName fieldName
  | namespace = mkFieldName (nameSpaceField tName fieldName)
  | otherwise = mkFieldName fieldName

-- withSubscriptionField: t => SubscriptionField t
withSubscriptionField :: TypeKind -> Type -> Type
withSubscriptionField tKind x
  | isSubscription tKind = AppT (ConT ''SubscriptionField) x
  | otherwise = x

-- withArgs: t => a -> t
withArgs :: TypeName -> Type -> Type
withArgs argsTypename = AppT (AppT arrowType argType)
  where
    argType = ConT $ mkTypeName argsTypename
    arrowType = ConT ''Arrow

-- withMonad: t => m t
withMonad :: Type -> Type
withMonad = AppT m'

------------------------------------------------
withFieldWrappers ::
  TypeKind ->
  Maybe (FieldContent TRUE cat) ->
  Type ->
  Type
withFieldWrappers kind (Just (FieldArgs ArgumentsDefinition {argumentsTypename = Just argsTypename})) =
  withArgs argsTypename
    . withSubscriptionField kind
    . withMonad
withFieldWrappers kind _
  | isOutputObject kind =
    withSubscriptionField kind
      . withMonad
  | otherwise = id

type Arrow = (->)
