{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.Schema.Schema where

import           Data.Data                           (Data)
import           Data.Morpheus.Schema.Directive      (Directive)
import           Data.Morpheus.Schema.Internal.Types (TypeLib (..))
import           Data.Morpheus.Schema.Utils.Utils    (Type, createObjectType, typeFromObject)
import           GHC.Generics                        (Generic)

data Schema = Schema
  { types            :: [Type]
  , queryType        :: Maybe Type
  , mutationType     :: Maybe Type
  , subscriptionType :: Maybe Type
  , directives       :: [Directive]
  } deriving (Show, Data, Generic)

convertTypes :: TypeLib -> [Type]
convertTypes lib' = map typeFromObject (object lib')

initSchema :: TypeLib -> Schema
initSchema types' =
  Schema
    { types = convertTypes types'
    , queryType = Just $ createObjectType "Query" "Query Description" []
    , mutationType = Nothing
    , subscriptionType = Nothing
    , directives = []
    }
