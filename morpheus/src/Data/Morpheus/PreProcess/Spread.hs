{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Spread
    ( spreadFieldsWhile
    )
where

import           Data.Text                      ( Text )
import qualified Data.Map                      as M
                                                ( lookup )
import           Data.List                      ( find )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Morpheus.Error.Fragment   ( unknownFragment )
import           Data.Morpheus.Types.Types      ( Validation(..)
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , FragmentLib
                                                , Fragment(..)
                                                , GQLQueryRoot(..)
                                                , ParserPosition(..)
                                                )
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , ErrorLocation(..)
                                                )
import qualified Data.Text                     as T

shouldSpread :: [(Text, QuerySelection)] -> Bool
shouldSpread list = case find isFragment list of
    Just _  -> True
    Nothing -> False

isFragment :: (Text, QuerySelection) -> Bool
isFragment (key, Spread _ _) = True
isFragment (key, _         ) = False


balaba (ParserPosition pos) x =
    [GQLError { message = x, locations = [ErrorLocation pos 0] }]

spreadError :: ParserPosition -> MetaInfo -> [GQLError]
spreadError  loc meta = balaba loc $ T.concat ["Unknown fragment \"", key meta, "\"."]


validateSpread
    :: FragmentLib
    -> ParserPosition
    -> Text
    -> Validation [(Text, QuerySelection)]
validateSpread frags location key = case M.lookup key frags of
    Nothing -> Left $ spreadError location metaData
    Just (Fragment _ _ (SelectionSet _ gqlObj)) -> pure gqlObj
    where metaData = MetaInfo { className = "", cons = "", key = key }

propagateSpread
    :: GQLQueryRoot
    -> (Text, QuerySelection)
    -> Validation [(Text, QuerySelection)]
propagateSpread root (key, Spread _ location) =
    validateSpread (fragments root) location key
propagateSpread root (text, value) = pure [(text, value)]

spreadFields :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFields root selectors = concat <$> mapM (propagateSpread root) selectors

spreadFieldsWhile :: GQLQueryRoot -> SelectionSet -> Validation SelectionSet
spreadFieldsWhile root selectors = spreadFields root selectors >>= checkUpdate
  where
    checkUpdate x = if shouldSpread x then spreadFieldsWhile root x else pure x
