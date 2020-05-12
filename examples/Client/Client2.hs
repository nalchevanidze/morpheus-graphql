{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Client.Client2
  ( fetchHero
  , fetUser
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus.Client       (Fetch (..), defineByDocumentFile, defineByIntrospectionFile, gql)
import           Data.Morpheus.Types        (ScalarValue (..))

defineByIntrospectionFile
  "./examples/assets/introspection.json"
  [gql|
    # Query Hero with Compile time Validation
    subscription MySubscription
    {
      newFoo
      { email }
    }
  |]

