{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.JSON.Custom.Mutation
  ( test,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    FetchResult (..),
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
    mutation TestMutation
      {
        mutationTypeName
      }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "JSON/Custom/Mutation"

client :: IO (Either FetchError (FetchResult TestMutation))
client = fetch resolver ()

test :: TestTree
test = testCase "test Mutation" $ do
  value <- client
  assertEqual
    "test custom Mutation"
    ( Right
        ( FetchResult
          ( TestMutation
              { mutationTypeName = Just "TestMutation"
              }
          )
          []
        )
    )
    value
