{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection
  )
where


import           Control.Monad                  ((>=>))

-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( unknownSelectionField )
import           Data.Morpheus.Types.Internal.AST
                                                ( Selection(..)
                                                , SelectionContent(..)
                                                , Fragment(..)
                                                , SelectionSet
                                                , FieldsDefinition(..)
                                                , Name
                                                , RAW
                                                , VALID
                                                , SelectionSet
                                                , UnionTag(..)
                                                , Ref(..)
                                                , DataUnion
                                                )
import qualified Data.Morpheus.Types.Internal.AST.MergeSet as MS
                                                ( join )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..) 
                                                , selectOr
                                                , empty
                                                , singleton
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , askUnionMemberType
                                                , askScopeTypeName
                                                )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( castFragmentType
                                                , resolveSpread
                                                )


-- returns all Fragments used in Union
exploreUnionFragments
  :: [Name]
  -> Selection RAW
  -> Validation [Fragment]
exploreUnionFragments unionTags = splitFrag
 where
  packFragment fragment = [fragment]
  splitFrag
    :: Selection RAW -> Validation [Fragment]
  splitFrag (Spread ref) = packFragment <$> resolveSpread unionTags ref 
  splitFrag Selection { selectionName = "__typename",selectionContent = SelectionField } = pure []
  splitFrag Selection { selectionName, selectionPosition } = do
    typeName <- askScopeTypeName
    failure $ unknownSelectionField typeName (Ref selectionName selectionPosition)
  splitFrag (InlineFragment fragment) = packFragment <$>
    castFragmentType Nothing (fragmentPosition fragment) unionTags fragment

-- sorts Fragment by contitional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments
  :: [TypeDef] -> [Fragment] -> [(TypeDef, [Fragment])]
tagUnionFragments types fragments 
    = filter notEmpty 
    $ map categorizeType types
 where
  notEmpty = not . null . snd
  categorizeType :: (Name, FieldsDefinition) -> (TypeDef, [Fragment])
  categorizeType datatype = (datatype, filter matches fragments)
    where matches fragment = fragmentType fragment == fst datatype

type TypeDef = (Name, FieldsDefinition)

clusterTypes :: Ref -> SelectionSet RAW -> DataUnion ->Validation [(TypeDef, [Fragment])]
clusterTypes selectionRef selectionSet members = do
  -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
  -- for example
  -- User | Admin | Product
  unionTypes <- traverse (askUnionMemberType selectionRef) members
  let unionTags = map fst unionTypes
  -- find all Fragments used in Selection
  spreads <- concat <$> traverse (exploreUnionFragments unionTags) (toList selectionSet)
  -- 
  pure $ tagUnionFragments unionTypes spreads


{-
    - all Variable and Fragment references will be: resolved and validated
    - unionTypes: will be clustered under type names
      ...A on T1 {<SelectionA>}
      ...B on T2 {<SelectionB>}
      ...C on T2 {<SelectionC>}
      will be become : [
          UnionTag "T1" {<SelectionA>},
          UnionTag "T2" {<SelectionB>,<SelectionC>}
      ]
 -}
validateCluster
      :: (TypeDef -> SelectionSet RAW -> Validation (SelectionSet VALID))
      -> SelectionSet RAW
      -> [(TypeDef, [Fragment])]
      -> Validation (SelectionContent VALID)
validateCluster validator __typename = traverse _validateCluster >=> fmap UnionSelection . fromList
 where
  _validateCluster :: (TypeDef, [Fragment]) -> Validation UnionTag
  _validateCluster  (unionType, fragmets) = do
        fragmentSelections <- MS.join (__typename:map fragmentSelection fragmets)
        UnionTag (fst unionType) <$> validator unionType fragmentSelections

validateUnionSelection 
    :: (TypeDef -> SelectionSet RAW -> Validation (SelectionSet VALID)) 
    -> Ref
    -> SelectionSet RAW 
    -> DataUnion
    -> Validation (SelectionContent VALID)
validateUnionSelection validate selectionRef selectionSet members = do
    let (__typename :: SelectionSet RAW) = selectOr empty singleton "__typename" selectionSet
    categories <- clusterTypes
        selectionRef
        selectionSet 
        members
    validateCluster validate __typename categories