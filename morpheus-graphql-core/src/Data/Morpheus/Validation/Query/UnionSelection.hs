{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Query.UnionSelection
  ( validateUnionSelection,
    validateInterfaceSelection,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Mergeable (OrdMap)
import Data.Morpheus.Internal.Utils
  ( empty,
    fromElems,
    mergeConcat,
    selectOr,
    startHistory,
  )
import Data.Morpheus.Types.Internal.AST.Base (Position (..))
import Data.Morpheus.Types.Internal.AST.Name (TypeName)
import Data.Morpheus.Types.Internal.AST.Selection
  ( Fragment (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    SelectionSet,
    UnionTag (..),
  )
import Data.Morpheus.Types.Internal.AST.Stage (RAW, VALID)
import Data.Morpheus.Types.Internal.AST.TypeCategory
  ( IMPLEMENTABLE,
    OUT,
    toCategory,
  )
import Data.Morpheus.Types.Internal.AST.TypeSystem
  ( TypeContent (..),
    TypeDefinition (..),
    UnionTypeDefinition,
    mkType,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    Scope (..),
    askInterfaceTypes,
    askTypeMember,
    asksScope,
  )
import Data.Morpheus.Validation.Query.Fragment
  ( ResolveFragment (resolveValidFragment),
    castFragmentType,
  )
import Relude hiding (empty, join)

-- returns all Fragments used for Possible Types
splitFragment ::
  (ResolveFragment s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  [TypeDefinition IMPLEMENTABLE VALID] ->
  Selection RAW ->
  FragmentValidator s (Either UnionTag (Selection RAW))
splitFragment _ _ x@Selection {} = pure (Right x)
splitFragment f types (Spread _ ref) = Left <$> resolveValidFragment f (typeName <$> types) ref
splitFragment f types (InlineFragment fragment@Fragment {fragmentType}) =
  Left . UnionTag fragmentType
    <$> ( castFragmentType Nothing (fragmentPosition fragment) (typeName <$> types) fragment
            >>= f
        )

exploreFragments ::
  (ResolveFragment s) =>
  ( Fragment RAW ->
    FragmentValidator s (SelectionSet VALID)
  ) ->
  OrdMap TypeName (TypeDefinition IMPLEMENTABLE VALID) ->
  SelectionSet RAW ->
  FragmentValidator s ([UnionTag], SelectionSet RAW)
exploreFragments validateFragment types selectionSet = do
  (tags, selections) <- partitionEithers <$> traverse (splitFragment validateFragment (toList types)) (toList selectionSet)
  selectionPosition <- fromMaybe (Position 0 0) <$> asksScope position
  (tags,)
    <$> fromElems
      ( ( Selection
            { selectionName = "__typename",
              selectionAlias = Nothing,
              selectionPosition,
              selectionArguments = empty,
              selectionContent = SelectionField,
              selectionDirectives = empty
            }
        )
          : selections
      )

-- sorts Fragment by conditional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments ::
  [UnionTag] ->
  OrdMap TypeName (TypeDefinition IMPLEMENTABLE VALID) ->
  HashMap TypeName [SelectionSet VALID]
tagUnionFragments fragments types = fmap categorizeType getSelectedTypes
  where
    getSelectedTypes :: HashMap TypeName [TypeName]
    getSelectedTypes = fromList (map select fragments)
      where
        select UnionTag {unionTagName} =
          ( unionTagName,
            selectOr
              [unionTagName]
              getCompatibleTypes
              unionTagName
              types
          )
    categorizeType ::
      [TypeName] -> [SelectionSet VALID]
    categorizeType compatibleTypes =
      unionTagSelection
        <$> filter
          ((`elem` compatibleTypes) . unionTagName)
          fragments

getCompatibleTypes :: TypeDefinition a s -> [TypeName]
getCompatibleTypes TypeDefinition {typeName, typeContent = DataObject {objectImplements}} = typeName : objectImplements
getCompatibleTypes TypeDefinition {typeName} = [typeName]

joinClusters ::
  SelectionSet VALID ->
  HashMap TypeName [SelectionSet VALID] ->
  FragmentValidator s (SelectionContent VALID)
joinClusters selSet typedSelections
  | null typedSelections = pure (SelectionSet selSet)
  | otherwise =
    traverse mkUnionTag (HM.toList typedSelections)
      >>= fmap (UnionSelection selSet) . startHistory . fromElems
  where
    mkUnionTag :: (TypeName, [SelectionSet VALID]) -> FragmentValidator s UnionTag
    mkUnionTag (typeName, fragments) = UnionTag typeName <$> startHistory (mergeConcat (selSet :| fragments))

validateInterfaceSelection ::
  ResolveFragment s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  TypeDefinition IMPLEMENTABLE VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateInterfaceSelection
  validateFragment
  validate
  typeDef@TypeDefinition {typeName}
  inputSelectionSet = do
    possibleTypes <- askInterfaceTypes typeDef
    (spreads, selectionSet) <- exploreFragments validateFragment possibleTypes inputSelectionSet
    validSelectionSet <- validate typeDef selectionSet
    let tags = tagUnionFragments spreads possibleTypes
    let interfaces = selectOr [] id typeName tags
    defaultSelection <- startHistory $ mergeConcat (validSelectionSet :| interfaces)
    joinClusters defaultSelection (HM.delete typeName tags)

mkUnionRootType :: FragmentValidator s (TypeDefinition IMPLEMENTABLE VALID)
mkUnionRootType = (`mkType` DataObject [] empty) <$> asksScope currentTypeName

validateUnionSelection ::
  ResolveFragment s =>
  (Fragment RAW -> FragmentValidator s (SelectionSet VALID)) ->
  (TypeDefinition IMPLEMENTABLE VALID -> SelectionSet RAW -> FragmentValidator s (SelectionSet VALID)) ->
  UnionTypeDefinition OUT VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateUnionSelection validateFragment validate members selectionSet = do
  unionTypes <- traverse (fmap toCategory . askTypeMember) members
  (spreads, selSet) <- exploreFragments validateFragment unionTypes selectionSet
  typeDef <- mkUnionRootType
  validSelection <- validate typeDef selSet
  joinClusters validSelection (tagUnionFragments spreads unionTypes)
