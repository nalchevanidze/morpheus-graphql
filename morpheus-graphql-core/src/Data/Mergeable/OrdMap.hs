{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.OrdMap
  ( OrdMap (..),
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Mergeable.Internal.Merge (Merge (..))
import Data.Mergeable.Internal.NameCollision (NameCollision (..))
import Data.Mergeable.Internal.Resolution
  ( Indexed (..),
    indexed,
  )
import Data.Mergeable.IsMap
  ( FromList (..),
    IsMap (..),
  )
import Data.Morpheus.Ext.Empty (Empty (..))
import Data.Morpheus.Ext.Failure
  ( Failure,
  )
import Data.Morpheus.Types.Internal.AST.Error (ValidationErrors)
import Language.Haskell.TH.Syntax (Lift (..))
import Relude
  ( ($),
    (.),
    (<$>),
    Eq,
    Foldable (foldMap, toList),
    Functor (fmap),
    HashMap,
    Hashable,
    Monad,
    Show,
    Traversable,
    map,
    sortOn,
  )

-- OrdMap
newtype OrdMap k a = OrdMap
  { mapEntries :: HashMap k (Indexed k a)
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Traversable,
      Empty
    )

instance (Lift a, Lift k, Eq k, Hashable k) => Lift (OrdMap k a) where
  lift (OrdMap x) = [|OrdMap (HM.fromList ls)|]
    where
      ls = HM.toList x

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (OrdMap x) = [||OrdMap (HM.fromList ls)||]
    where
      ls = HM.toList x
#endif

instance (Eq k, Hashable k) => Foldable (OrdMap k) where
  foldMap f = foldMap f . getElements

getElements :: (Eq k, Hashable k) => OrdMap k b -> [b]
getElements = fmap indexedValue . sortOn index . toList . mapEntries

instance (Eq k, Hashable k) => IsMap k (OrdMap k) where
  unsafeFromList = OrdMap . HM.fromList . fmap withKey . indexed
    where
      withKey idx = (indexedKey idx, idx)
  singleton k x = OrdMap $ HM.singleton k (Indexed 0 k x)
  lookup key OrdMap {mapEntries} = indexedValue <$> lookup key mapEntries

instance (NameCollision a, Eq k, Hashable k, Monad m, Failure ValidationErrors m) => Merge m (OrdMap k a) where
  merge (OrdMap x) (OrdMap y) = OrdMap <$> merge x y

instance
  ( NameCollision a,
    Monad m,
    Failure ValidationErrors m,
    Hashable k,
    Eq k
  ) =>
  FromList m OrdMap k a
  where
  fromList = fmap OrdMap . fromList . map xyz . indexed
    where
      xyz x = (indexedKey x, x)
