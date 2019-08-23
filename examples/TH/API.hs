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
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)

-- MORPHEUS
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (gqlDoc)
import           Data.Morpheus.Kind         (ENUM, INPUT_OBJECT, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types        (GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, IORes, Resolver,
                                             ScalarValue (..), resolver)

data MyUnion m
  = USER (User m)
  | ADDRESS (Address m)
  deriving (Generic)

instance Typeable a => GQLType (MyUnion a) where
  type KIND (MyUnion a) = UNION

data Euro =
  Euro Int
       Int
  deriving (Show, Generic)

instance GQLType Euro where
  type KIND Euro = SCALAR

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

data Address m = Address
  { city        :: () -> m Text
  , street      :: Text
  , houseNumber :: Int
  } deriving (Generic)

instance Typeable m => GQLType (Address m) where
  type KIND (Address m) = OBJECT

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [[Maybe [ID]]]
  , cityID  :: CityID
  } deriving (Generic)

data User m = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs -> m (Address m)
  , myUnion :: () -> m (MyUnion m)
  , home    :: CityID
  } deriving (Generic)

instance Typeable a => GQLType (User a) where
  type KIND (User a) = OBJECT
  description _ = "Custom Description for Client Defined User Type"

[gqlDoc|
   # GraphQL  Types Generated By Template Haskell

   type SomeObject {
     someName(arg1:Int): String!
     somePower: Int
   }

   union SomeUnion = SomeObject

   enum CityID {
      Paris
      BLN
      HH
   }

   input Coordinates {
      latitude : Euro!
      longitude: [[UID!]]!
   }

   input UID {
     uid: Text
   }
|]

bo :: SomeObject
bo = SomeObject {someName = const $ return "", somePower = const $ return (Just 1)}

fetchUser :: Monad m => m (Either String (User (Resolver m)))
fetchUser =
  return $
  Right $
  User {name = "George", email = "George@email.com", address, home = HH, myUnion = const $ return $ USER unionUser}
  where
    address :: Monad m => a -> m (Address m)
    address _ = return $ Address (const $ return "") "" 0
    unionAddress = Address {city = const $ return "Hamburg", street = "Street", houseNumber = 20}
    unionUser =
      User
        { name = "David"
        , email = "David@email.com"
        , address
        , home = BLN
        , myUnion = const $ return $ ADDRESS unionAddress
        }

newtype Query = Query
  { user :: () -> IORes (User IORes)
  } deriving (Generic)

gqlRoot :: GQLRootResolver IO () () Query () ()
gqlRoot =
  GQLRootResolver
    { queryResolver = return Query {user = const $ resolver fetchUser}
    , mutationResolver = return ()
    , subscriptionResolver = return ()
    }

thApi :: B.ByteString -> IO B.ByteString
thApi = interpreter gqlRoot
