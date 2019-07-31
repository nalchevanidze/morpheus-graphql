{-# LANGUAGE DeriveGeneric #-}

import    Data.Morpheus.Types  (ResM)
import    GHC.Generics  (Generic)

data Query = Query 
  { deity :: ArgDeity -> ResM (Deity)
  ,  character :: ArgCharacter -> ResM (Character)
  } deriving (Generic)

data ArgDeity = ArgDeity 
  { name :: Maybe [Maybe [Maybe [[Maybe [String]]]]]
  ,  mythology :: Maybe String
  } deriving (Generic)

data ArgCharacter = ArgCharacter 
  { characterID :: String
  ,  age :: Maybe Int
  } deriving (Generic)

data City = 
  Athens
  | Ithaca
  | Sparta
  | Troy deriving (Generic)

data Power = Power  Int

data Realm = Realm 
  { owner :: String
  ,  place :: Maybe Int
  } deriving (Generic)

data Deity = Deity 
  { fullName :: () -> ResM (String)
  ,  power :: () -> ResM (Maybe String)
  } deriving (Generic)

data Creature = Creature 
  { creatureName :: () -> ResM (String)
  ,  abilities :: () -> ResM (Maybe String)
  } deriving (Generic)

data Human = Human 
  { humanName :: () -> ResM (String)
  ,  profession :: () -> ResM (Maybe String)
  } deriving (Generic)

data Character = 
  Character_CREATURE Creature
  | Character_DEITY Deity
  | Character_HUMAN Human deriving (Generic)