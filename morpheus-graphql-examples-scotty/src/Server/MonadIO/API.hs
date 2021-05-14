{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Server.MonadIO.API where

import Control.Concurrent.STM
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.FileEmbed (makeRelativeToProject)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types.Internal.AST (OperationType)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Tuple.Extra (both)
import Network.HTTP.Types (Status (..))
import Web.Scotty
import Prelude hiding (id)

importGQLDocument =<< makeRelativeToProject "src/Server/MonadIO/schema.graphql"

-------------------------------------------------------------------------------
data DogRow = DogRow
  { dogId :: Int,
    dogName :: Text,
    ownerId :: Int
  }

type DogTable = [DogRow]

data UserRow = UserRow
  { userId :: Int,
    userUsername :: Text,
    userPassword :: Text,
    userFullName :: Text
  }

type UserTable = [UserRow]

data FollowRow = FollowRow
  { followerId :: Int,
    followeeId :: Int
  }

type FollowTable = [FollowRow]

-------------------------------------------------------------------------------
data Database = Database
  { userTable :: UserTable,
    dogTable :: DogTable,
    followTable :: FollowTable
  }

dbInit :: Database
dbInit =
  Database
    { dogTable =
        [ DogRow 1 "Golden Retriever" 1, --
          DogRow 2 "Labrador" 3,
          DogRow 2 "Chihu" 1
        ],
      userTable =
        [ UserRow 1 "user1" "123" "Jon Snow",
          UserRow 2 "user2" "123" "Night King",
          UserRow 3 "user3" "123" "Arya Stark"
        ],
      followTable =
        [ FollowRow 1 2, --
          FollowRow 1 3
        ]
    }

tokenUser1 :: Text
tokenUser1 = "aGVsbG8gd29ybGQKZGFzZmFzZGZhZGY="

tokenUser2 :: Text
tokenUser2 = "ZaHsJld3fJhZGZkYXN2m239ZGFzZg=="

tokenUser3 :: Text
tokenUser3 = "ZGtzamZkbGFrc2pmZmRhZmVy223="

-------------------------------------------------------------------------------
type ErrorCode = Int

type Headers = [(Text, Text)] -- Could be anything

data Env = Env
  { database :: TVar Database,
    reqHeaders :: Headers
  }

newtype Web a = Web
  { runWeb :: ReaderT Env (ExceptT ErrorCode IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Int,
      MonadIO
    )

-------------------------------------------------------------------------------

-- | Resolve value
type Value (o :: OperationType) (a :: k) = ResolverO o () Web a

-- | Resolve (f value)
type Composed (o :: OperationType) f (a :: k) = ComposedResolver o () Web f a

-------------------------------------------------------------------------------
getDB :: Value o Database
getDB = do
  dbTVar <- lift $ asks database
  liftIO (readTVarIO dbTVar)

-------------------------------------------------------------------------------
requireAuthorized :: Value o Int
requireAuthorized = do
  reqHeaders <- lift $ asks reqHeaders
  case find ((== "Authorization") . fst) reqHeaders of
    Just (_, token)
      | token == tokenUser1 -> return 1
      | token == tokenUser2 -> return 2
      | token == tokenUser2 -> return 3
    _ -> fail "Unauthorized"

-------------------------------------------------------------------------------
rootResolver :: RootResolver Web () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { login = loginResolver,
            getUser = getUserResolver,
            dogs = dogsResolver
          },
      mutationResolver = Mutation {addDog = addDogResolver},
      subscriptionResolver = Undefined
    }

-------------------------------------------------------------------------------
loginResolver :: LoginArgs -> Composed QUERY Maybe Session
loginResolver LoginArgs {username, password} = do
  users <- fmap userTable getDB
  let match user =
        userUsername user == username && userPassword user == password
  case find match users of
    Just userRow -> do
      let tokenUser
            | userId userRow == 1 = tokenUser1
            | userId userRow == 2 = tokenUser2
            | otherwise = tokenUser3
      pure
        $ Just
        $ Session {token = pure tokenUser, user = userResolver userRow}
    Nothing -> fail "Invalid user or password"

getUserResolver :: GetUserArgs -> Composed o Maybe User
getUserResolver GetUserArgs {id} = do
  _ <- requireAuthorized
  users <- fmap userTable getDB
  case find ((== id) . userId) users of
    Just userRow -> do
      user <- userResolver userRow
      pure $ Just user
    _ -> pure Nothing

dogsResolver :: Composed QUERY [] Dog
dogsResolver = do
  _ <- requireAuthorized
  dogs <- fmap dogTable getDB
  traverse dogResolver dogs

-------------------------------------------------------------------------------
addDogResolver :: AddDogArgs -> Value MUTATION Dog
addDogResolver AddDogArgs {name} = do
  currentUserId <- requireAuthorized
  db <- getDB
  let dogs = dogTable db
      nxtId = (+ 1) . maximum . fmap dogId $ dogs
      dogToAdd = DogRow nxtId name currentUserId
      newDogs = dogToAdd : dogs
      newDb = db {dogTable = newDogs}
  dbTVar <- lift $ asks database
  liftIO $ atomically $ writeTVar dbTVar newDb
  dogResolver dogToAdd

-------------------------------------------------------------------------------
userResolver :: UserRow -> Value o User
userResolver UserRow {userId = thisUserId, userFullName} =
  pure $
    User
      { id = idResolver,
        name = nameResolver,
        favoriteDog = favoriteDogResolver,
        follows = followsResolver
      }
  where
    idResolver = pure thisUserId
    nameResolver = pure userFullName
    favoriteDogResolver :: Composed o Maybe Dog
    favoriteDogResolver = do
      dogs <- fmap dogTable getDB
      -- the 1st dog is the favorite dog
      case find ((== thisUserId) . ownerId) dogs of
        Just dogRow -> do
          dog <- dogResolver dogRow
          return . Just $ dog
        Nothing -> return Nothing
    followsResolver :: Composed o [] User
    followsResolver = do
      follows <- fmap followTable getDB
      users <- fmap userTable getDB
      let userFolloweeIds =
            map followeeId . filter ((== thisUserId) . followerId) $ follows
      let userFollowees = filter ((`elem` userFolloweeIds) . userId) users
      traverse userResolver userFollowees

dogResolver :: DogRow -> Value o Dog
dogResolver (DogRow dogId dogName ownerId) =
  pure $ Dog {id = idResolver, name = nameResolver, owner = ownerResolver}
  where
    idResolver = pure dogId
    nameResolver = pure dogName
    ownerResolver :: Value o User
    ownerResolver = do
      users <- fmap userTable getDB
      let userRow = fromJust . find ((== ownerId) . userId) $ users
      userResolver userRow

-------------------------------------------------------------------------------
api :: B.ByteString -> Web B.ByteString
api = interpreter rootResolver

app :: IO ()
app = do
  db <- newTVarIO dbInit
  scotty 8080
    $ post "/api"
    $ do
      reqBody <- body
      reqHeaders <- headers
      let env = Env db $ map (both $ T.pack . LT.unpack) reqHeaders
      res <-
        liftIO . runExceptT . flip runReaderT env . runWeb $ api reqBody
      case res of
        Left code -> status $ Status code "Error"
        Right rawResponse -> raw rawResponse
