{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.JSON.Custom.Subscription
  ( test,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    gql,
  )
import Data.Text (Text)
import Spec.Utils
  ( defineClientWithJSON,
    mockApi,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )
import Prelude
  ( ($),
    Either (..),
    IO,
    Maybe (..),
    String,
  )

defineClientWithJSON
  "JSON/Custom"
  [gql|
    subscription TestSubscription
      {
        subscriptionTypeName
      }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "JSON/Custom/Subscription"

client :: IO (Either (FetchError TestSubscription) TestSubscription)
client = fetch resolver ()

test :: TestTree
test = testCase "test Subscription" $ do
  value <- client
  assertEqual
    "test custom Subscription"
    ( Right
        ( TestSubscription
            { subscriptionTypeName =
                Just "TestSubscription"
            }
        )
    )
    value
