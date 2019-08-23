{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE TypeOperators      #-}

module TH.API
  ( thApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

-- MORPHEUS
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (gqlDoc)
import           Data.Morpheus.Kind         (ENUM, INPUT_OBJECT, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types        (GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, IORes,
                                             ScalarValue (..))

data MyUnion
  = USER User
  | ADDRESS Address
  deriving (Generic)

instance GQLType MyUnion where
  type KIND MyUnion = UNION

data Euro =
  Euro Int
       Int
  deriving (Show, Generic)

instance GQLType Euro where
  type KIND Euro = SCALAR

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [[Maybe [ID]]]
  , cityID  :: CityID
  } deriving (Generic)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs -> IORes Address
  , myUnion :: () -> IORes MyUnion
  , home    :: CityID
  } deriving (Generic)

instance GQLType User where
  type KIND User = OBJECT
  description _ = "Custom Description for Client Defined User Type"

[gqlDoc|
   # GraphQL  Types Generated By Template Haskell

   type Address {
     city        : Text!
     street      : Text!
     houseNumber : Int!
   }

   union SomeUnion = Address

   enum CityID {
      Paris
      BLN
      HH
   }

   input Coordinates {
      latitude  : Euro!
      longitude : [[UID!]]!
   }

   input UID {
     uid: Text
   }
|]


fetchUser :: IORes User
fetchUser =
  return $
  User {name = "George", email = "George@email.com", address, home = HH, myUnion = const $ return $ USER unionUser}
  where
    address :: a -> IORes Address
    address _ = return simpleAddress
    simpleAddress =
      Address {city = const $ return "Hamburg", street = const $ return "Street", houseNumber = const $ return 20}
    unionUser =
      User
        { name = "David"
        , email = "David@email.com"
        , address
        , home = BLN
        , myUnion = const $ return $ ADDRESS simpleAddress
        }

newtype Query = Query
  { user :: () -> IORes User
  } deriving (Generic)

gqlRoot :: GQLRootResolver IO () () Query () ()
gqlRoot =
  GQLRootResolver
    { queryResolver = return Query {user = const fetchUser}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

thApi :: B.ByteString -> IO B.ByteString
thApi = interpreter gqlRoot
