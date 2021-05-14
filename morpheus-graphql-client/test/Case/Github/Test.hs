{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Github.Test
  ( testInterface,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    Fetch (..),
    FetchError,
    ScalarValue (..),
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
    Applicative (..),
    Bool (..),
    Either (..),
    Eq (..),
    IO,
    Maybe (..),
    Show,
    String,
  )

newtype GitTimestamp = GitTimestamp
  { unGitTimestamp :: Text
  }
  deriving (Eq, Show)

instance DecodeScalar GitTimestamp where
  decodeScalar (String x) = pure (GitTimestamp x)
  decodeScalar _ = Left ""

instance EncodeScalar GitTimestamp where
  encodeScalar (GitTimestamp x) = String x

defineClientWith
  "Github"
  [gql|
    query GetTags ($user: String!, $repo: String!)
      {
        repository(owner: $user, name: $repo) {
          refs(refPrefix: "refs/tags/", first: 100) {
            pageInfo {
              endCursor
              hasNextPage
            }
            edges {
              cursor
              node {
                name
                target {
                  __typename
                  ... on Tag {
                    tagger {
                      date
                    }
                  }
                }
              }
            }
          }
        }
      }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "Interface"

client :: IO (Either (FetchError GetTags) GetTags)
client =
  fetch
    resolver
    GetTagsArgs
      { user = "UserName",
        repo = "repoName"
      }

testInterface :: TestTree
testInterface = testCase "test Github interfaces" $ do
  value <- client
  assertEqual
    "test Github interface"
    ( Right
        ( GetTags
            { repository =
                Just
                  RepositoryRepository
                    { refs =
                        Just
                          RepositoryRefsRefConnection
                            { pageInfo =
                                RepositoryRefsPageInfoPageInfo
                                  { endCursor = Just "",
                                    hasNextPage = False
                                  },
                              edges =
                                Just
                                  [ Just
                                      RepositoryRefsEdgesRefEdge
                                        { cursor = "",
                                          node =
                                            Just
                                              RepositoryRefsEdgesNodeRef
                                                { name = "",
                                                  target =
                                                    Just
                                                      RepositoryRefsEdgesNodeTargetGitObject
                                                        { __typename = ""
                                                        }
                                                }
                                        }
                                  ]
                            }
                    }
            }
        )
    )
    value
