{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Sophisticated.API
  ( app,
    EVENT,
    root,
  )
where

import Control.Monad.Trans (lift)
-- MORPHEUS

import Data.FileEmbed (makeRelativeToProject)
import Data.Map (Map)
import qualified Data.Map as M
  ( fromList,
  )
import Data.Morpheus
  ( App,
    deriveApp,
  )
import Data.Morpheus.Document
  ( importGQLDocumentWithNamespace,
  )
import Data.Morpheus.Subscriptions
  ( Event (..),
    Hashable,
  )
import Data.Morpheus.Types
  ( DecodeScalar (..),
    EncodeScalar (..),
    GQLType (..),
    ID,
    MUTATION,
    Resolver,
    ResolverM,
    ResolverQ,
    ResolverS,
    RootResolver (..),
    ScalarValue (..),
    SubscriptionField,
    WithOperation,
    constRes,
    liftEither,
    publish,
    subscribe,
  )
import Data.Set (Set)
import qualified Data.Set as S
  ( fromList,
  )
import Data.Text
  ( Text,
    pack,
  )
import GHC.Generics (Generic)

newtype A a = A {wrappedA :: a}
  deriving (Generic, GQLType)

type AIntText = A (Int, Text)

type AText = A Text

type SetInt = Set Int

type MapTextInt = Map Text Int

importGQLDocumentWithNamespace =<< makeRelativeToProject "src/Server/Sophisticated/shared.gql"

importGQLDocumentWithNamespace =<< makeRelativeToProject "src/Server/Sophisticated/api.gql"

data Animal
  = AnimalCat Cat
  | AnimalDog Dog
  | AnimalBird Bird
  | Giraffe {giraffeName :: Text}
  | UnidentifiedSpecie
  deriving (Show, Generic, GQLType)

data Euro
  = Euro
      Int
      Int
  deriving (Show, Generic)

instance DecodeScalar Euro where
  decodeScalar (Int x) =
    pure
      ( Euro
          (round (fromIntegral x / 100 :: Double))
          (mod x 100)
      )
  decodeScalar _ = Left "invalid currency!"

instance EncodeScalar Euro where
  encodeScalar (Euro x y) = Int (x * 100 + y)

data Channel = USER | ADDRESS
  deriving
    ( Show,
      Eq,
      Ord,
      Generic,
      Hashable
    )

newtype Content = Content {contentID :: Int}

type EVENT = Event Channel Content

app :: App EVENT IO
app = deriveApp root

root :: RootResolver IO EVENT Query Mutation Subscription
root =
  RootResolver
    { queryResolver,
      mutationResolver,
      subscriptionResolver
    }
  where
    queryResolver =
      Query
        { queryUser = resolveUser,
          queryAnimal = resolveAnimal,
          querySet = const $ pure $ S.fromList [1, 2, 4],
          querySomeMap = pure $ M.fromList [("robin", 1), ("carl", 2)],
          queryWrapped1 = constRes $ A (0, "some value"),
          queryWrapped2 = pure $ A "",
          queryFail1 = fail "fail example!!",
          queryFail2 = liftEither alwaysFail,
          queryShared =
            pure
              SharedType
                { sharedTypeName = pure "some name"
                },
          queryTestInterface =
            pure
              Account
                { accountName = pure "Value from Interface!"
                },
          queryTestInput = pure . pack . show
        }
    -------------------------------------------------------------
    mutationResolver =
      Mutation
        { mutationCreateUser = resolveCreateUser,
          mutationCreateAddress = resolveCreateAddress,
          mutationSetAddress = resolveSetAddress
        }
    subscriptionResolver =
      Subscription
        { subscriptionNewUser = resolveNewUser,
          subscriptionNewAddress = const resolveNewAddress
        }

-- Resolve QUERY

alwaysFail :: IO (Either String a)
alwaysFail = pure $ Left "fail example"

resolveUser :: ResolverQ EVENT IO User
resolveUser = liftEither (getDBUser (Content 2))

resolveAnimal :: QueryAnimalArgs -> ResolverQ EVENT IO Text
resolveAnimal QueryAnimalArgs {queryAnimalArgsAnimal} =
  pure (pack $ show queryAnimalArgsAnimal)

-- Resolve MUTATIONS
--
-- Mutation With Event Triggering : sends events to subscription
resolveCreateUser :: ResolverM EVENT IO User
resolveCreateUser = do
  requireAuthorized
  publish [userUpdate]
  liftEither setDBUser

-- Mutation With Event Triggering : sends events to subscription
resolveCreateAddress :: ResolverM EVENT IO Address
resolveCreateAddress = do
  requireAuthorized
  publish [addressUpdate]
  lift setDBAddress

-- Mutation Without Event Triggering
resolveSetAddress :: ResolverM EVENT IO Address
resolveSetAddress = lift setDBAddress

-- Resolve SUBSCRIPTION
resolveNewUser :: SubscriptionField (ResolverS EVENT IO User)
resolveNewUser = subscribe USER $ do
  requireAuthorized
  pure subResolver
  where
    subResolver (Event _ content) = liftEither (getDBUser content)

resolveNewAddress :: SubscriptionField (ResolverS EVENT IO Address)
resolveNewAddress = subscribe ADDRESS $ do
  requireAuthorized
  pure subResolver
  where
    subResolver (Event _ content) = lift (getDBAddress content)

-- Events ----------------------------------------------------------------
addressUpdate :: EVENT
addressUpdate = Event [ADDRESS] (Content {contentID = 10})

userUpdate :: EVENT
userUpdate = Event [USER] (Content {contentID = 12})

-- DB::Getter --------------------------------------------------------------------
getDBAddress :: WithOperation o => Content -> IO (Address (Resolver o EVENT IO))
getDBAddress _id = do
  city <- dbText
  street <- dbText
  number <- dbInt
  pure
    Address
      { addressCity = pure city,
        addressStreet = pure street,
        addressHouseNumber = pure number
      }

getDBUser :: WithOperation o => Content -> IO (Either String (User (Resolver o EVENT IO)))
getDBUser _ = do
  Person {name, email} <- dbPerson
  pure $
    Right
      User
        { userName = pure name,
          userEmail = pure email,
          userAddress = const $ lift (getDBAddress (Content 12)),
          userOffice = constRes Nothing,
          userHome = pure CityIDHH,
          userEntity =
            pure
              [ MyUnionAddress
                  Address
                    { addressCity = pure "city",
                      addressStreet = pure "street",
                      addressHouseNumber = pure 1
                    },
                MyUnionUser
                  User
                    { userName = pure name,
                      userEmail = pure email,
                      userAddress = const $ lift (getDBAddress (Content 12)),
                      userOffice = constRes Nothing,
                      userHome = pure CityIDHH,
                      userEntity = pure []
                    }
              ]
        }

-- DB::Setter --------------------------------------------------------------------
setDBAddress :: IO (Address (Resolver MUTATION EVENT IO))
setDBAddress = do
  city <- dbText
  street <- dbText
  houseNumber <- dbInt
  pure
    Address
      { addressCity = pure city,
        addressStreet = pure street,
        addressHouseNumber = pure houseNumber
      }

setDBUser :: IO (Either String (User (Resolver MUTATION EVENT IO)))
setDBUser = do
  Person {name, email} <- dbPerson
  pure $ Right $
    User
      { userName = pure name,
        userEmail = pure email,
        userAddress = const $ lift setDBAddress,
        userOffice = constRes Nothing,
        userHome = pure CityIDHH,
        userEntity = pure []
      }

-- DB ----------------------
data Person = Person
  { name :: Text,
    email :: Text
  }

dbText :: IO Text
dbText = pure "Updated Text"

dbInt :: IO Int
dbInt = pure 11

dbPerson :: IO Person
dbPerson = pure Person {name = "George", email = "George@email.com"}

requireAuthorized :: WithOperation o => Resolver o e IO ()
requireAuthorized = pure ()
