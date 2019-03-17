{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Generics.GQLMutation
  ( GQLMutation(..)
  , NoMutation(..)
  ) where

import           Data.Data                              (Data, Typeable)
import qualified Data.Map                               as M
import           Data.Morpheus.Generics.DeriveResolvers (DeriveResolvers (..), resolveBySelection)
import           Data.Morpheus.Generics.TypeRep         (Selectors (..), resolveTypes)
import           Data.Morpheus.Schema.Utils.Utils       (Field, TypeLib, createType, emptyLib)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (initialMeta)
import           Data.Morpheus.Types.Types              (QuerySelection (..), ResolveIO)
import           Data.Proxy
import           GHC.Generics

class GQLMutation a where
  encodeMutation :: a -> QuerySelection -> ResolveIO JSType
  default encodeMutation :: (Generic a, Data a, DeriveResolvers (Rep a), Show a) =>
    a -> QuerySelection -> ResolveIO JSType
  encodeMutation rootResolver (SelectionSet _ sel _pos) =
    resolveBySelection sel $ deriveResolvers initialMeta $ from rootResolver
  mutationSchema :: a -> TypeLib
  default mutationSchema :: (Generic a, Data a) =>
    a -> TypeLib
  mutationSchema _ = introspectMutation (Proxy :: Proxy a)
  introspectMutation :: Proxy a -> TypeLib
  default introspectMutation :: (Show a, Selectors (Rep a) Field, Typeable a) =>
    Proxy a -> TypeLib
  introspectMutation _ = resolveTypes mutationType types
    where
      mutationType = M.fromList [("Mutation", createType "Mutation" fields)]
      fieldTypes = getFields (Proxy :: Proxy (Rep a))
      types = map snd fieldTypes
      fields = map fst fieldTypes

data NoMutation =
  NoMutation

instance GQLMutation NoMutation where
  encodeMutation _ _ = pure JSNull
  mutationSchema _ = emptyLib
  introspectMutation _ = emptyLib
