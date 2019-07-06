{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.Holistic.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (ENUM, INPUT_OBJECT, KIND, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types        (GQLRootResolver (..), GQLScalar (..), GQLType (..), ID (..), ResM,
                                             ScalarValue (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND TestEnum = ENUM

type instance KIND TestScalar = SCALAR

type instance KIND NestedInputObject = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND TestInputObject = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND User = OBJECT

type instance KIND TestUnion = UNION

data TestEnum
  = EnumA
  | EnumB
  | EnumC
  deriving (Generic, GQLType)

data TestScalar =
  TestScalar Int
             Int
  deriving (Generic, GQLType)

instance GQLScalar TestScalar where
  parseValue _ = pure (TestScalar 1 0)
  serialize (TestScalar x y) = Int (x * 100 + y)

newtype NestedInputObject = NestedInputObject
  { fieldTestID :: ID
  } deriving (Generic, GQLType)

data TestInputObject = TestInputObject
  { fieldTestScalar        :: TestScalar
  , fieldNestedInputObject :: [Maybe NestedInputObject]
  } deriving (Generic, GQLType)

data StreetArgs = StreetArgs
  { argInputObject :: TestInputObject
  , argMaybeString :: Maybe Text
  } deriving (Generic)

data Address = Address
  { city        :: Text
  , street      :: StreetArgs -> ResM (Maybe [Maybe [[[Text]]]])
  , houseNumber :: Int
  } deriving (Generic, GQLType)

data TestUnion
  = UnionA User
  | UnionB Address
  deriving (Generic, GQLType)

data Coordinates = Coordinates
  { latitude  :: TestScalar
  , longitude :: Int
  } deriving (Generic, GQLType)

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [Int]
  , cityID  :: TestEnum
  } deriving (Generic)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs -> ResM Address
  , office  :: OfficeArgs -> ResM Address
  , friend  :: () -> ResM (Maybe User)
  } deriving (Generic)

instance GQLType User where
  description _ = "Custom Description for Client Defined User Type"

data Query = Query
  { user      :: () -> ResM User
  , testUnion :: Maybe TestUnion
  } deriving (Generic)

newtype Mutation = Mutation
  { createUser :: AddressArgs -> ResM User
  } deriving (Generic)

newtype Subscription = Subscription
  { newUser :: AddressArgs -> ResM User
  } deriving (Generic)

resolveAddress :: a -> ResM Address
resolveAddress _ = return Address {city = "", houseNumber = 1, street = const $ return Nothing}

resolveUser :: a -> ResM User
resolveUser _ =
  return $
  User
    {name = "testName", email = "", address = resolveAddress, office = resolveAddress, friend = const $ return Nothing}

rootResolver :: GQLRootResolver IO () Query Mutation Subscription
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {user = resolveUser, testUnion = Nothing}
    , mutationResolver = return Mutation {createUser = resolveUser}
    , subscriptionResolver = return Subscription {newUser = resolveUser}
    }

api :: ByteString -> IO ByteString
api = interpreter rootResolver
