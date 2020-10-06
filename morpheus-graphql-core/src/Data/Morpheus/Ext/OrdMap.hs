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
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.OrdMap
  ( OrdMap (..),
    unsafeFromList,
  )
where

-- MORPHEUS
import Control.Monad (Monad)
import Data.Foldable (Foldable (..))
import Data.Functor ((<$>), Functor (..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (sortOn)
import Data.Maybe (maybe)
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Ext.Elems (Elems (..))
import Data.Morpheus.Ext.Map
  ( Indexed (..),
    indexed,
  )
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Failure,
    FromElems (..),
    KeyOf (..),
    Selectable (..),
    SemigroupM (..),
    toPair,
  )
import Data.Morpheus.Types.Internal.AST.Base (ValidationErrors)
import Data.Traversable (Traversable (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( ($),
    (.),
    Eq,
    Show,
  )

-- OrdMap
newtype OrdMap k a = OrdMap
  { mapEntries :: HashMap k (Indexed k a)
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Traversable
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

instance (KeyOf k a, Hashable k) => Collection a (OrdMap k a) where
  empty = OrdMap HM.empty
  singleton x = OrdMap $ HM.singleton (keyOf x) (Indexed 0 (keyOf x) x)

instance (Eq k, Hashable k) => Selectable k a (OrdMap k a) where
  selectOr fb f key OrdMap {mapEntries} = maybe fb (f . indexedValue) (HM.lookup key mapEntries)

instance (NameCollision a, Monad m, KeyOf k a, Failure ValidationErrors m) => SemigroupM m (OrdMap k a) where
  mergeM ref (OrdMap x) (OrdMap y) = OrdMap <$> mergeM ref x y

instance (NameCollision a, Monad m, Failure ValidationErrors m, KeyOf k a, Hashable k) => FromElems m a (OrdMap k a) where
  fromElems values = OrdMap <$> fromElems (indexed (toPair <$> values))

instance (Eq k, Hashable k) => Elems a (OrdMap k a) where
  elems = getElements

unsafeFromList ::
  (Hashable k, Eq k) =>
  [(k, a)] ->
  OrdMap k a
unsafeFromList = OrdMap . HM.fromList . fmap withKey . indexed
  where
    withKey idx = (indexedKey idx, idx)
