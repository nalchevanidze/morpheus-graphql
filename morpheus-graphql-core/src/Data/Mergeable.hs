module Data.Mergeable
  ( Merge (..),
    mergeNoDuplicates,
    recursiveMerge,
    mergeConcat,
    Indexed (..),
    NameCollision (..),
    ResolutionT,
    fromListT,
    indexed,
    resolveWith,
    runResolutionT,
    collect,
    IsMap (..),
    MergeMap,
    toNonEmpty,
    OrdMap,
  )
where

import Data.Mergeable.Internal.Merge
import Data.Mergeable.Internal.NameCollision
import Data.Mergeable.Internal.Resolution
import Data.Mergeable.IsMap
import Data.Mergeable.MergeMap
import Data.Mergeable.OrdMap
