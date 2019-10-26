{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Types.Internal.AST.RawSelection
  ( Reference
  , Argument(..)
  , RawArgument(..)
  , RawSelection(..)
  , Fragment(..)
  , RawSelection'(..)
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  ) where


import           Language.Haskell.TH.Syntax                 (Lift (..))

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Selection (Argument (..))
import           Data.Morpheus.Types.Internal.Base          (Collection, Key, Position, Reference)
import           Data.Morpheus.Types.Internal.TH            (apply, liftText, liftTextMap, liftTextTuple)

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show)

instance Lift Fragment where
  lift (Fragment t p sel) = apply 'Fragment [liftText t, lift p, liftTextMap sel]

data RawSelection' a = RawSelection'
  { rawSelectionArguments :: RawArguments
  , rawSelectionPosition  :: Position
  , rawSelectionAlias     :: Maybe Reference
  , rawSelectionRec       :: a
  } deriving (Show)

instance Lift a => Lift (RawSelection' a) where
  lift (RawSelection' t p alias sel) = apply 'RawSelection' [liftTextMap t, lift p, lift alias , lift sel]

type FragmentLib = [(Key, Fragment)]

data RawArgument
  = VariableReference Reference
  | RawArgument Argument
  deriving (Show, Lift)

type RawArguments = Collection RawArgument

type RawSelectionSet = Collection RawSelection

instance Lift RawSelection where
  lift (RawSelectionSet (RawSelection' t p alias sel)) =
    apply 'RawSelectionSet [apply 'RawSelection' [liftTextMap t, lift p, lift alias,liftTextMap sel]]
  lift (RawSelectionField p) = apply 'RawSelectionField [lift p]
  lift (InlineFragment f) = apply 'InlineFragment [lift f]
  lift (Spread f) = apply 'Spread [lift f]
  lift (RawAlias p s) = apply 'RawAlias [lift p, liftTextTuple s]

data RawSelection
  = RawSelectionSet (RawSelection' RawSelectionSet)
  | RawSelectionField (RawSelection' ())
  | InlineFragment Fragment
  | Spread Reference
  | RawAlias { rawAliasPosition  :: Position
             , rawAliasSelection :: (Key, RawSelection) }
  deriving (Show)
