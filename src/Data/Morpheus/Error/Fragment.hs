{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself
  , unusedFragment
  , unknownFragment
  , cannotBeSpreadOnType
  , fragmentNameCollision
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.Base
                                                ( Reference(..)
                                                , Position
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLError(..)
                                                , GQLErrors
                                                )

{-
  FRAGMENT:
    type Experience {
        experience ( lang: LANGUAGE ) : String ,
        date: String
    }
    fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
    fragment H on T1 { ...A} , fragment A on T { ...H } -> "Cannot spread fragment \"H\" within itself via A."
    fragment H on D {...}  ->  "Unknown type \"D\"."
    {...H} -> "Unknown fragment \"H\"."
-}
fragmentNameCollision :: [Reference] -> GQLErrors
fragmentNameCollision = map toError
 where
  toError Reference { refName, refPosition } = GQLError
    { desc      = "There can be only one fragment named \"" <> refName <> "\"."
    , positions = [refPosition]
    }

unusedFragment :: [Reference] -> GQLErrors
unusedFragment = map toError
 where
  toError Reference { refName, refPosition } = GQLError
    { desc      = "Fragment \"" <> refName <> "\" is never used."
    , positions = [refPosition]
    }

cannotSpreadWithinItself :: [Reference] -> GQLErrors
cannotSpreadWithinItself fragments =
  [GQLError { desc = text, positions = map refPosition fragments }]
 where
  text = T.concat
    [ "Cannot spread fragment \""
    , refName $ head fragments
    , "\" within itself via "
    , T.intercalate "," (map refName fragments)
    , "."
    ]

-- {...H} -> "Unknown fragment \"H\"."
unknownFragment :: Text -> Position -> GQLErrors
unknownFragment key' position' = errorMessage position' text
  where text = T.concat ["Unknown Fragment \"", key', "\"."]

-- Fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
cannotBeSpreadOnType :: Maybe Text -> Text -> Position -> Text -> GQLErrors
cannotBeSpreadOnType key' type' position' selectionType' = errorMessage
  position'
  text
 where
  text = T.concat
    [ "Fragment"
    , getName key'
    , " cannot be spread here as objects of type \""
    , selectionType'
    , "\" can never be of type \""
    , type'
    , "\"."
    ]
  getName (Just x') = T.concat [" \"", x', "\""]
  getName Nothing   = ""
