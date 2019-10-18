{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Mythology.API
  ( mythologyApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B

import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Types        (GADTResolver (..), GQLRootResolver (..), GQLType, QUERY, Undefined (..),
                                             resolver)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Mythology.Character.Deity  (Deity (..), dbDeity)

newtype Query m = Query
  { deity :: DeityArgs -> m Deity
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> GADTResolver QUERY IO e Deity
resolveDeity args = QueryResolver $ resolver $ dbDeity (name args) (mythology args)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = pure Query {deity = resolveDeity}
    , mutationResolver = pure Undefined
    , subscriptionResolver = pure Undefined
    }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
