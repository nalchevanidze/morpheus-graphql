{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Resolve.Introspect
  ( introspect
  ) where

import           Data.Morpheus.Kind                     (ENUM, INPUT_OBJECT, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Resolve.Generics.TypeRep (ObjectRep (..), RecSel, SelOf, UnionRep (..), resolveTypes)
import           Data.Morpheus.Resolve.Internal         (Context (..), EnumConstraint, InputObjectConstraint, InputOf,
                                                         Intro_, ObjectConstraint, OutputOf, UnionConstraint)
import           Data.Morpheus.Schema.Type              (DeprecationArgs)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import qualified Data.Morpheus.Types.GQLArgs            as Args (GQLArgs (..))
import           Data.Morpheus.Types.GQLScalar          (GQLScalar (..))
import           Data.Morpheus.Types.GQLType            (GQLType (..))
import           Data.Morpheus.Types.Internal.Data      (DataArguments, DataField (..), DataFullType (..),
                                                         DataLeaf (..), DataTypeWrapper (..), DataValidator)
import           Data.Morpheus.Types.Resolver           (Resolver (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text, pack)
import           GHC.Generics

scalarTypeOf :: GQLType a => DataValidator -> Proxy a -> DataFullType
scalarTypeOf validator = Leaf . LeafScalar . buildType validator

enumTypeOf :: GQLType a => [Text] -> Proxy a -> DataFullType
enumTypeOf tags' = Leaf . LeafEnum . buildType tags'

-- |   Generates internal GraphQL Schema for query validation and introspection render
-- * 'kind': object, scalar, enum ...
-- * 'args': type of field arguments
--    * '()' for 'input values' , they are just JSON properties and does not have any argument
--    * 'DataArguments' for field Resolvers Types, where 'DataArguments' is type of arguments
class Introspect a kind args where
  __field :: Context a kind args -> Text -> DataField args
    --   generates data field representation of object field
    --   according to parameter 'args' it could be
    --   * input object field: if args is '()'
    --   * object: if args is 'DataArguments'
  introspect :: Intro_ a kind args -- Generates internal GraphQL Schema

type OutputConstraint a = Introspect a (KIND a) DataArguments

{--

  Introspect SCALAR Types: SCALAR, ENUM

-}
introspectEnum ::
     forall a f. (GQLType a, EnumRep (Rep a))
  => Intro_ a (KIND a) f
introspectEnum _ = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) [] (Proxy @a)

instance (GQLScalar a, GQLType a) => Introspect a SCALAR DataArguments where
  __field _ = field_ SCALAR (Proxy @a) []
  introspect _ = updateLib (scalarTypeOf (scalarValidator $ Proxy @a)) [] (Proxy @a)

instance EnumConstraint a => Introspect a ENUM DataArguments where
  __field _ = field_ ENUM (Proxy @a) []
  introspect _ = introspectEnum (Context :: OutputOf a)

instance (GQLScalar a, GQLType a) => Introspect a SCALAR () where
  __field _ = field_ SCALAR (Proxy @a) ()
  introspect _ = updateLib (scalarTypeOf (scalarValidator $ Proxy @a)) [] (Proxy @a)

instance EnumConstraint a => Introspect a ENUM () where
  __field _ = field_ ENUM (Proxy @a) ()
  introspect _ = introspectEnum (Context :: InputOf a)

{--

  Introspect OBJECT Types:  OBJECTS , INPUT_OBJECT

-}
instance ObjectConstraint a => Introspect a OBJECT DataArguments where
  __field _ = field_ OBJECT (Proxy @a) []
  introspect _ = updateLib (OutputObject . buildType fields') stack' (Proxy @a)
    where
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

instance InputObjectConstraint a => Introspect a INPUT_OBJECT () where
  __field _ = field_ INPUT_OBJECT (Proxy @a) ()
  introspect _ = updateLib (InputObject . buildType fields') stack' (Proxy @a)
    where
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

-- | recursion for Object types, both of them : 'INPUT_OBJECT' and 'OBJECT'
-- iterates on field types  and introspects them recursively
instance (Selector s, Introspect a (KIND a) f) => ObjectRep (RecSel s a) f where
  objectFieldTypes _ =
    [((name, __field (Context :: Context a (KIND a) f) name), introspect (Context :: Context a (KIND a) f))]
    where
      name = pack $ selName (undefined :: SelOf s)

{--

  Introspect UNION Types:  UNION

-}
-- | recursion for union types
-- iterates on possible types for UNION and introspects them recursively
instance (OutputConstraint a, ObjectConstraint a) => UnionRep (RecSel s a) where
  possibleTypes _ = [(field_ OBJECT (Proxy @a) () "", introspect (Context :: OutputOf a))]

instance UnionConstraint a => Introspect a UNION DataArguments where
  __field _ = field_ UNION (Proxy @a) []
  introspect _ = updateLib (Union . buildType fields) stack (Proxy @a)
    where
      (fields, stack) = unzip $ possibleTypes (Proxy @(Rep a))

{--

  Introspect WRAPPER Types: Maybe, LIST , Resolver

-}
instance Introspect a (KIND a) f => Introspect (Maybe a) WRAPPER f where
  __field _ name = maybeField $ __field (Context :: Context a (KIND a) f) name
    where
      maybeField :: DataField f -> DataField f
      maybeField field@DataField {fieldTypeWrappers = NonNullType:xs} = field {fieldTypeWrappers = xs}
      maybeField field                                                = field
  introspect _ = introspect (Context :: Context a (KIND a) f)

instance Introspect a (KIND a) f => Introspect [a] WRAPPER f where
  __field _ name = listField (__field (Context :: Context a (KIND a) f) name)
    where
      listField :: DataField f -> DataField f
      listField x = x {fieldTypeWrappers = [NonNullType, ListType] ++ fieldTypeWrappers x}
  introspect _ = introspect (Context :: Context a (KIND a) f)

-- | Introspection Of Resolver ' a ::-> b'
-- introspects 'a' as argument and 'b' as output type
instance (OutputConstraint a, Args.GQLArgs p) => Introspect (Resolver c p a) WRAPPER DataArguments where
  __field _ name = (__field (Context :: OutputOf a) name) {fieldArgs = map fst $ Args.introspect (Proxy @p)}
  introspect _ typeLib = resolveTypes typeLib $ inputTypes' ++ [introspect (Context :: OutputOf a)]
    where
      inputTypes' = map snd $ Args.introspect (Proxy @p)

instance Args.GQLArgs DeprecationArgs
