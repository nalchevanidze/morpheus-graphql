{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Client.DefineByIntrospection
  ( fetchUsers,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    Fetch (..),
    FetchError,
    ScalarValue (..),
    defineByIntrospectionFile',
    gql,
  )
import Data.Semigroup ((<>))
import Data.Text (Text)
import Prelude
  ( ($),
    (*),
    (+),
    Applicative (..),
    Either,
    Eq,
    IO,
    Int,
    Show,
    String,
    print,
    putStrLn,
  )

data Euro
  = Euro
      Int
      Int
  deriving (Show, Eq)

instance EncodeScalar Euro where
  encodeScalar (Euro x y) = Int (x * 101 + y)

instance DecodeScalar Euro where
  decodeScalar _ = pure (Euro 1 0)

defineByIntrospectionFile'
  (makeRelativeToProject "assets/introspection.json")
  [gql|

    # Query Hero with Compile time Validation
    query GetUser ($coordinates: Coordinates!)
      {
        myUser: user {
           boo3: name
           myUserEmail: email
           address (coordinates: $coordinates ){
             city
           }
           customAdress: address (coordinates: $coordinates ){
               customCity: city
           }
        }
        user {
          email
        }
      }
  |]

usersApi :: ByteString -> IO ByteString
usersApi req = do
  print req
  putStrLn ""
  pure $
    "{\"data\":{"
      <> "\"myUser\":{ "
      <> "   \"boo3\": \"name\","
      <> "   \"myUserEmail\": \"some field\","
      <> "   \"address\":{ \"city\":\"some city\" },"
      <> "   \"customAdress\":{ \"customCity\":\"some custom city\" }"
      <> "},"
      <> " \"user\":{ \"email\":\"some email\" }"
      <> "}}"

fetchUsers :: IO (Either (FetchError GetUser) GetUser)
fetchUsers = fetch usersApi userArgs
  where
    userArgs :: Args GetUser
    userArgs =
      GetUserArgs
        { coordinates =
            Coordinates
              { longitude = [],
                latitude = Euro 1 25
              }
        }
