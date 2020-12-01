{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
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
    DataUnion,
    DataInputUnion,
    UnionMember (..),
    mkInputUnionFields,
  )
where

import Data.Morpheus.Internal.Utils
  ( Failure (..),
    KeyOf (..),
    elems,
    selectBy,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName (..),
    Message (..),
    Msg (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    toFieldName,
  )
import Data.Morpheus.Types.Internal.AST.Fields
  ( FieldDefinition (..),
    FieldsDefinition,
    unsafeFromFields,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( Stage,
  )
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( IN,
    OUT,
    TypeCategory,
  )
import Data.Morpheus.Types.Internal.AST.Value
  ( Object,
    ObjectEntry (..),
    Value (..),
  )
import Language.Haskell.TH.Syntax (Lift (..))
import Relude

mkUnionMember :: TypeName -> UnionMember cat s
mkUnionMember name = UnionMember name False

mkNullaryMember :: TypeName -> UnionMember cat s
mkNullaryMember name = UnionMember name True

data UnionMember (cat :: TypeCategory) (s :: Stage) = UnionMember
  { memberName :: TypeName,
    nullary :: Bool
  }
  deriving (Show, Lift, Eq)

type DataUnion s = [UnionMember OUT s]

type DataInputUnion s = [UnionMember IN s]

instance RenderGQL (UnionMember cat s) where
  render = render . memberName

instance Msg (UnionMember cat s) where
  msg = msg . memberName

instance KeyOf TypeName (UnionMember cat s) where
  keyOf = memberName

constraintInputUnion ::
  forall stage schemaStage.
  [UnionMember IN schemaStage] ->
  Object stage ->
  Either Message (UnionMember IN schemaStage, Value stage)
constraintInputUnion tags hm =
  case elems hm of
    [] -> failure ("empy for input Union was not Provided." :: Message)
    [ObjectEntry (FieldName name) value] ->
      (,value) <$> isPossibleInputUnion tags (TypeName name)
    _ -> failure ("input union can have only one variant." :: Message)

isPossibleInputUnion :: [UnionMember IN s] -> TypeName -> Either Message (UnionMember IN s)
isPossibleInputUnion tags name =
  selectBy
    (msg name <> " is not possible union type")
    name
    tags

mkInputUnionFields :: [UnionMember IN s] -> FieldsDefinition IN s
mkInputUnionFields = unsafeFromFields . fmap mkInputUnionField

mkInputUnionField :: UnionMember IN s -> FieldDefinition IN s
mkInputUnionField UnionMember {memberName, nullary} =
  FieldDefinition
    { fieldName = toFieldName memberName,
      fieldDescription = Nothing,
      fieldContent = Nothing,
      fieldType =
        TypeRef
          { typeConName,
            typeWrappers = [TypeMaybe],
            typeArgs = Nothing
          },
      fieldDirectives = []
    }
  where
    typeConName
      | nullary = "Empty"
      | otherwise = memberName
