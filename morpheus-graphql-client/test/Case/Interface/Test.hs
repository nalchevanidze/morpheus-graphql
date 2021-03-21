{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Interface.Test
  ( testInterface,
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
    Either (..),
    IO,
    String,
  )

defineClientWith
  "Interface"
  [gql|
    query MyQuery {
      character {
        name
        ... on Deity {
              power
        }

        ... on Hero {
              hobby
        }
      }
      character2: character {
        name1: name
        name
      }
      character3: character {
        ... on Hero {
              hobby
        }
        ... on Character {
              name2: name
        }
      }
      character4: character {
        ... on Hero {
          hobby
        }
      }
    }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "Interface"

client :: IO (Either FetchError (FetchResult MyQuery))
client = fetch resolver ()

testInterface :: TestTree
testInterface = testCase "test interfaces" $ do
  value <- client
  assertEqual
    "test interface"
    ( Right
        ( FetchResult
            ( MyQuery
              { character =
                  [ CharacterDeity
                      { name = "Deity Name",
                        power = "Deity Power",
                        __typename = "Deity"
                      },
                    CharacterCharacter
                      { name = "Character Name",
                        __typename = "Character"
                      },
                    CharacterHero
                      { name = "Hero Name",
                        hobby = "Deity Power",
                        __typename = "Hero"
                      }
                  ],
                character2 =
                  [ Character2Character
                      { name1 = "test name",
                        name = "test name"
                      }
                  ],
                character3 =
                  [ Character3Hero
                      { hobby = "Hero Hobby",
                        name2 = "Hero name2",
                        __typename = "Hero"
                      },
                    Character3Deity
                      { name2 = "Hero name2",
                        __typename = "Deity"
                      },
                    Character3Character
                      { name2 = "Character name2",
                        __typename = "Character"
                      }
                  ],
                character4 =
                  [ Character4Character
                      { __typename = "Character"
                      },
                    Character4Hero
                      { hobby = "Hero Hobby",
                        __typename = "Hero"
                      },
                    Character4Character
                      { __typename = "Deity"
                      }
                  ]
              }
          )
          []
        )
    )
    value
