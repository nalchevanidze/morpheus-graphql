{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Kind.GQLQuery
  ( GQLQuery(..)
  ) where

import           Data.Data                              (Data, Typeable)
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Kind.GQLObject           (GQLObject (..))
import           Data.Morpheus.Schema.Internal.Types    (Core (..), GObject (..), ObjectField, TypeLib, initTypeLib)
import           Data.Morpheus.Schema.Schema            (Schema, initSchema)
import           Data.Morpheus.Types.Error              (ResolveIO)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (initialMeta)
import           Data.Morpheus.Types.Query.Selection    (SelectionSet)
import           Data.Proxy
import           Data.Text                              (Text)
import           GHC.Generics

class GQLQuery a where
  encodeQuery :: a -> TypeLib -> SelectionSet -> ResolveIO JSType
  default encodeQuery :: (Generic a, Data a, DeriveResolvers (Rep a), Show a) =>
    a -> TypeLib -> SelectionSet -> ResolveIO JSType
  encodeQuery rootResolver types sel = resolveBySelection sel (schemaResolver ++ resolvers)
    where
      schemaResolver = [("__schema", (`encode` initSchema types))] -- TODO : lazy schema derivation
      resolvers = deriveResolvers initialMeta $ from rootResolver
  querySchema :: a -> TypeLib
  default querySchema :: (Selectors (Rep a) (Text, ObjectField), Typeable a) =>
    a -> TypeLib
  querySchema _ = resolveTypes typeLib stack
    where
      typeLib = introspect (Proxy @Schema) queryType
      queryType = initTypeLib ("Query", GObject fields (Core "Query" "Description"))
      fieldTypes = getFields (Proxy @(Rep a))
      stack = map snd fieldTypes
      fields = map fst fieldTypes
