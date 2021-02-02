{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified Case.Enum.Test as Enum
import Case.Interface.Test (testInterface)
import qualified Case.JSON.Custom.Errors as JSONCustomErrors
import qualified Case.JSON.Custom.Mutation as JSONCustomMutation
import qualified Case.JSON.Custom.Query as JSONCustomQuery
import qualified Case.JSON.Custom.Subscription as JSONCustomSubscription
import Case.LowercaseTypeName.Test
  ( testLowercaseTypeName,
  )
import Test.Tasty
  ( defaultMain,
    testGroup,
  )
import Prelude
  ( ($),
    IO,
  )

main :: IO ()
main =
  defaultMain $
    testGroup
      "client tests"
      [ testInterface,
        testLowercaseTypeName,
        Enum.test,
        JSONCustomErrors.test,
        JSONCustomMutation.test,
        JSONCustomQuery.test,
        JSONCustomSubscription.test
      ]
