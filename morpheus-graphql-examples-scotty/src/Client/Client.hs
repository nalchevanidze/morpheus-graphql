{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Client
  ( fetchUser,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    FetchResult,
    defineByDocumentFile,
    defineByDocumentFile',
    gql,
  )
import Data.Morpheus.Types
  ( DecodeScalar (..),
    EncodeScalar (..),
    ScalarValue (..),
  )
import Data.Text (Text)
import Prelude

data Euro
  = Euro
      Int
      Int
  deriving (Show, Eq)

instance DecodeScalar Euro where
  decodeScalar _ = pure (Euro 1 0)

instance EncodeScalar Euro where
  encodeScalar (Euro x y) = Int (x * 101 + y)

defineByDocumentFile'
  (makeRelativeToProject "src/Server/Sophisticated/api.gql")
  [gql|
    # Subscription Test Query
    subscription MySubscription
    {
      newUser
      { subEmail : email }
    }
  |]

defineByDocumentFile'
  (makeRelativeToProject "src/Server/Sophisticated/api.gql")
  [gql|
    # Query Hero with Compile time Validation
    query GetUser ($coordinates: Coordinates!)
      {
        myUser: user {
           name
           aliasEmail: email
           address (coordinates: $coordinates ){
             city
           }
           aliasAdress: address (coordinates: $coordinates ){
              city
           }
        }
        user {
          email
          name
        }
      }
  |]

fetchUser :: (ByteString -> IO ByteString) -> IO (Either FetchError (FetchResult GetUser))
fetchUser = flip fetch args
  where
    args :: Args GetUser
    args =
      GetUserArgs
        { coordinates =
            Coordinates
              { longitude = [],
                latitude = Euro 1 2
              }
        }
