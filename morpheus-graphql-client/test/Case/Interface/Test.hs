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
        __typename
        ... on Deity {
              power
        }
        ... on Hero {
              hoby
        }
      }
      character2: character {
        name1: name
        name
      }

      character3: character {
        ... on Hero {
              hoby
        }
        ... on Character {
              name2: name
        }
      }
      character4: character {
        ... on Hero {
              hoby
        }
      }
    }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "Interface"

client :: IO (Either String MyQuery)
client = fetch resolver ()

testInterface :: TestTree
testInterface = testCase "test interfaces" $ do
  value <- client
  assertEqual
    "test interface"
    ( Right
        ( MyQuery
            { character =
                [ CharacterDeity
                    { name = "Deity Name",
                      power = "Deity Power",
                      __typename = "Deity"
                    },
                  CharacterCharacter
                    { name = "Charatcer Name",
                      __typename = "Character"
                    },
                  CharacterHero
                    { name = "Hero Name",
                      hoby = "Deity Power",
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
                    { hoby = "Hero Hoby",
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
                    { hoby = "Hero Hoby",
                      __typename = "Hero"
                    }
                ]
            }
        )
    )
    value
