{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Union
  ( constraintInputUnion,
    mkUnionMember,
    mkNullaryMember,
    UnionTypeDefinition,
    UnionMember (..),
    mkInputUnionFields,
    getInputUnionValue,
  )
where

import Data.Mergeable (NameCollision (..), OrdMap)
import Data.Morpheus.Internal.Utils
  ( Empty (empty),
    Failure (..),
    KeyOf (..),
    selectBy,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Message (..),
    Msg (..),
  )
import Data.Morpheus.Types.Internal.AST.Error (msgValidation)
import Data.Morpheus.Types.Internal.AST.Fields
  ( FieldDefinition (..),
    FieldsDefinition,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( TypeName,
    unitTypeName,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( Stage,
  )
import Data.Morpheus.Types.Internal.AST.Type
  ( TypeRef (..),
    mkMaybeType,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( IN,
    TypeCategory,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( Object,
    ObjectEntry (..),
    Value (..),
  )
import Language.Haskell.TH.Syntax (Lift (..))
import Relude hiding (empty)

mkUnionMember :: TypeName -> UnionMember cat s
mkUnionMember name = UnionMember name False

mkNullaryMember :: TypeName -> UnionMember cat s
mkNullaryMember name = UnionMember name True

data UnionMember (cat :: TypeCategory) (s :: Stage) = UnionMember
  { memberName :: TypeName,
    nullary :: Bool
  }
  deriving (Show, Lift, Eq)

instance NameCollision (UnionMember c s) where
  nameCollision UnionMember {memberName} =
    "There can Be only one union variant named "
      <> msgValidation memberName

type UnionTypeDefinition k s = OrdMap TypeName (UnionMember k s)

instance RenderGQL (UnionMember cat s) where
  renderGQL = renderGQL . memberName

instance Msg (UnionMember cat s) where
  msg = msg . memberName

instance KeyOf TypeName (UnionMember cat s) where
  keyOf = memberName

getInputUnionValue ::
  forall stage.
  Object stage ->
  Either Message (TypeName, Value stage)
getInputUnionValue hm =
  case toList hm of
    [] -> Left "Exclusive input objects must provide a value for at least one field."
    [ObjectEntry name value] -> pure (coerce name, value)
    _ -> failure ("Exclusive input objects are not allowed to provide values for multiple fields." :: Message)

constraintInputUnion ::
  forall stage schemaStage.
  UnionTypeDefinition IN schemaStage ->
  Object stage ->
  Either Message (UnionMember IN schemaStage, Value stage)
constraintInputUnion tags hm = do
  (name, value) <- getInputUnionValue hm
  (,value) <$> isPossibleInputUnion tags name

isPossibleInputUnion :: UnionTypeDefinition IN s -> TypeName -> Either Message (UnionMember IN s)
isPossibleInputUnion tags name =
  selectBy
    (msg name <> " is not possible union type")
    name
    tags

mkInputUnionFields :: Foldable t => t (UnionMember IN s) -> FieldsDefinition IN s
mkInputUnionFields = unsafeFromFields . fmap mkInputUnionField . toList

mkInputUnionField :: UnionMember IN s -> FieldDefinition IN s
mkInputUnionField UnionMember {memberName, nullary} =
  FieldDefinition
    { fieldName = coerce memberName,
      fieldDescription = Nothing,
      fieldContent = Nothing,
      fieldType =
        TypeRef
          { typeConName,
            typeWrappers = mkMaybeType
          },
      fieldDirectives = empty
    }
  where
    typeConName
      | nullary = unitTypeName
      | otherwise = memberName
