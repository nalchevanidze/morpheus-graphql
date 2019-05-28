{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Deprecated.API
  ( gqlApi
  ) where

import           Data.Morpheus       (InputAction, OutputAction, streamInterpreter)
import           Data.Morpheus.Kind  (ENUM, GQLArgs, GQLMutation, GQLQuery, GQLScalar (..), GQLSubscription,
                                      GQLType (..), INPUT_OBJECT, KIND, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types ((::->) (..), GQLRoot (..), ID, ScalarValue (..))
import           Data.Text           (Text, pack)
import           Deprecated.Model    (JSONAddress, JSONUser, jsonAddress, jsonUser)
import qualified Deprecated.Model    as M (JSONAddress (..), JSONUser (..))
import           GHC.Generics        (Generic)

type instance KIND CityID = ENUM

type instance KIND Euro = SCALAR

type instance KIND UID = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND User = OBJECT

type instance KIND MyUnion = UNION

data MyUnion
  = USER User
  | ADDRESS Address
  deriving (Generic, GQLType)

data CityID
  = Paris
  | BLN
  | HH
  deriving (Generic, GQLType)

data Euro =
  Euro Int
       Int
  deriving (Generic, GQLType)

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

newtype UID = UID
  { uid :: Text
  } deriving (Show, Generic, GQLType)

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [Maybe [[UID]]]
  } deriving (Generic)

instance GQLType Coordinates where
  description _ = "just random latitude and longitude"

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  , owner       :: Maybe User
  } deriving (Generic, GQLType)

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic, GQLArgs)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [[Maybe [ID]]]
  , cityID  :: CityID
  } deriving (Generic, GQLArgs)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs ::-> Address
  , office  :: OfficeArgs ::-> Address
  , myUnion :: () ::-> MyUnion
  , home    :: CityID
  } deriving (Generic)

instance GQLType User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query = Query
  { user :: () ::-> User
  } deriving (Generic, GQLQuery)

newtype Mutation = Mutation
  { createUser :: () ::-> User
  } deriving (Generic, GQLMutation)

newtype Subscription = Subscription
  { newUser :: () ::-> User
  } deriving (Generic, GQLSubscription)

fetchAddress :: Euro -> Text -> IO (Either String Address)
fetchAddress _ streetName = do
  address' <- jsonAddress
  pure (transformAddress streetName <$> address')

transformAddress :: Text -> JSONAddress -> Address
transformAddress street' address' =
  Address {city = M.city address', houseNumber = M.houseNumber address', street = street', owner = Nothing}

resolveAddress :: AddressArgs ::-> Address
resolveAddress = Resolver $ \args -> fetchAddress (Euro 1 0) (pack $ show $ longitude $ coordinates args)

addressByCityID :: CityID -> Int -> IO (Either String Address)
addressByCityID Paris code = fetchAddress (Euro 1 code) "Paris"
addressByCityID BLN code   = fetchAddress (Euro 1 code) "Berlin"
addressByCityID HH code    = fetchAddress (Euro 1 code) "Hamburg"

resolveOffice :: JSONUser -> OfficeArgs ::-> Address
resolveOffice _ = Resolver $ \args -> addressByCityID (cityID args) 12

resolveUser :: () ::-> User
resolveUser = transformUser <$> Resolver (const jsonUser)

transformUser :: JSONUser -> User
transformUser user' =
  User
    { name = M.name user'
    , email = M.email user'
    , address = resolveAddress
    , office = resolveOffice user'
    , home = HH
    , myUnion =
        return $
        USER
          (User
             "unionUserName"
             "unionUserMail"
             resolveAddress
             (resolveOffice user')
             (return $ ADDRESS (Address "unionAdressStreet" "unionAdresser" 1 Nothing))
             HH)
    }

createUserMutation :: () ::-> User
createUserMutation = transformUser <$> Resolver (const jsonUser)

newUserSubscription :: () ::-> User
newUserSubscription = transformUser <$> Resolver (const jsonUser)

{-
  data Channels = UserAdded (Async User) | AddressAdded (Async Address)

  data Subscription = Subscription {
      newUser :: Async User,
      newAddress :: Async Address
  }

  -- resolveNewUserSubscription :: Async User
  -- resolveSubscription = async (UserAdded Pending)

  async :: Channels -> Stream Channels
  async = ....

  newtype Async a = Pending | Response { unpackAwait :: Stream Channels } |
-}
gqlApi :: InputAction a Text -> IO (OutputAction a Text)
gqlApi =
  streamInterpreter
    GQLRoot
      { query = Query {user = resolveUser}
      , mutation = Mutation {createUser = createUserMutation}
      , subscription = Subscription {newUser = newUserSubscription}
      }
