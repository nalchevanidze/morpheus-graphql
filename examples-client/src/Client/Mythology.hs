{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Mythology
  ( fetchHero,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( Fetch (..),
    ScalarValue (..),
    defineByDocumentFile,
    defineByIntrospectionFile,
    gql,
  )
import Data.Text (Text)

defineByDocumentFile
  "./assets/mythology.gql"
  [gql|
    # Query Hero with Compile time Validation
    query GetHero ($god: Realm, $someID: String!)
      {
        deity (mythology:$god) {
          power
          fullName
        }
        character(characterID: $someID ) {
          ...on Creature {
            name
            immortality
          }
          ...on Human {
            lifetime
            profession
          }
        }
        char2: character(characterID: $someID ) {
          ...on Creature {
              cName: name
          }
          ...on Human {
              lTime: lifetime
              prof: profession
          }
        }
      }
  |]

mythologyApi :: ByteString -> IO ByteString
mythologyApi req = do
  print req
  putStrLn ""
  return
    "{\"data\":{\"deity\":{ \"fullName\": \"name\" }, \"character\":{ \"__typename\":\"Human\", \"lifetime\": \"Lifetime\", \"profession\": \"Artist\" } ,  \"char2\":{ \"__typename\":\"Human\", \"lTime\": \"time\", \"prof\": \"Artist\" }  }}"

fetchHero :: IO (Either String GetHero)
fetchHero =
  fetch
    mythologyApi
    GetHeroArgs
      { getHeroArgsGod =
          Just
            Realm
              { realmOwner = "Zeus",
                realmAge = Just 10,
                realmRealm = Nothing,
                realmProfession = Just ProfessionArtist
              },
        getHeroArgsSomeID = "Hercules"
      }
