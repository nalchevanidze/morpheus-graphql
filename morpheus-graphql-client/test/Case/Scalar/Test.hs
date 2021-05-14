{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Scalar.Test
  ( test,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Client
  ( Fetch (..),
    gql,
  )
import Data.Text (Text)
import Spec.Utils
  ( defineClientWith,
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
    Bool (True),
    Double,
    Either (..),
    IO,
    Int,
    String,
  )

defineClientWith
  "Scalar"
  [gql|
    query MyQuery(
      $inputBolean: Boolean!
      $inputInt: Int!
      $inputFloat: Float!
      $inputString: String!
    ) {
      booleanResolver(booleanValue: $inputBolean)
      intResolver(intValue: $inputInt)
      floatResolver(booleanValue: $inputFloat)
      stringResolver(stringValue: $inputString)
    }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "Scalar"

-- GraphQL Boolean types must be represented with Haskell Bool types
testBoolean :: Bool
testBoolean = True

-- GraphQL Int types must be represented with Haskell Int types
testInt :: Int
testInt = 1242345

-- GraphQL Float types must be represented with Haskell Double types
testFloat :: Double
testFloat = 21233.1234145

-- GraphQL String types must be represented with Haskell Text types
testText :: Text
testText = "Athens"

client :: IO (Either String MyQuery)
client =
  fetch
    resolver
    MyQueryArgs
      { inputBolean = testBoolean,
        inputInt = testInt,
        inputFloat = testFloat,
        inputString = testText
      }

test :: TestTree
test = testCase "test Scalar" $ do
  value <- client
  assertEqual
    "test Scalar"
    ( Right
        ( MyQuery
            { booleanResolver = testBoolean,
              intResolver = testInt,
              floatResolver = testFloat,
              stringResolver = testText
            }
        )
    )
    value
