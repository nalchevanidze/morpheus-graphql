{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Morpheus.Types.Internal.Subscription.ClientConnectionStore
  ( ID
  , SesionID
  , ClientConnection(..)
  , ClientConnectionStore
  , empty
  , insert
  , adjust
  , delete
  , publish
  )
where

import           Data.List                      ( intersect )
import           Data.Foldable                  ( traverse_ )
import           Data.ByteString.Lazy.Char8     (ByteString)
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.HashMap.Lazy              ( HashMap , keys)
import qualified Data.HashMap.Lazy   as  HM     ( empty
                                                , insert
                                                , delete
                                                , adjust
                                                , elems
                                                , toList
                                                )


-- MORPHEUS
import           Data.Morpheus.Types.Internal.Subscription.Apollo
                                                ( toApolloResponse
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                (Empty(..))
import           Data.Morpheus.Types.Internal.Resolving
                                                ( SubEvent 
                                                , Event(..)
                                                , GQLChannel(..)
                                                )

type ID = UUID

type SesionID = Text

data ClientConnection e ( m :: * -> * ) =
  ClientConnection
    { connectionId       :: ID
    , connectionCallback :: ByteString -> m ()
    -- one connection can have multiple subsciprion session
    , connectionSessions :: HashMap SesionID (SubEvent e m)
    }

instance Show (ClientConnection e m) where
  show ClientConnection { connectionId, connectionSessions } =
    "Connection { id: "
      <> show connectionId
      <> ", sessions: "
      <> show (keys connectionSessions)
      <> " }"

publish
  :: ( Eq (StreamChannel event)
     , GQLChannel event
     , Monad m
     ) 
  => event 
  -> ClientConnectionStore event m 
  -> m ()
publish event = traverse_ sendMessage . elems
 where
  sendMessage ClientConnection { connectionSessions, connectionCallback  }
    | null connectionSessions  = pure ()
    | otherwise = traverse_ send (filterByChannels connectionSessions)
   where
    send (sid, Event { content = subscriptionRes }) 
      = toApolloResponse sid <$> subscriptionRes event >>= connectionCallback
    ---------------------------
    filterByChannels = filter
      ( not
      . null
      . intersect (streamChannels event)
      . channels
      . snd
      ) . HM.toList


-- stores active client connections
-- every registered client has ID
-- when client connection is closed client(including all its subsciprions) can By removed By its ID
newtype ClientConnectionStore e ( m :: * -> * ) = 
    ClientConnectionStore 
      { unpackStore :: HashMap ID (ClientConnection e m)
      } deriving (Show)

type StoreMap e m
  = ClientConnectionStore e m 
  -> ClientConnectionStore e m

mapStore 
  ::  ( HashMap ID (ClientConnection e m) 
      -> HashMap ID (ClientConnection e m)
      )
  -> StoreMap e m
mapStore f = ClientConnectionStore . f . unpackStore

elems :: ClientConnectionStore e m ->  [ClientConnection e m]
elems = HM.elems . unpackStore

instance Empty (ClientConnectionStore e m) where
  empty = ClientConnectionStore HM.empty

insert 
  :: ID
  -> (ByteString -> m ())
  -> StoreMap e m
insert connectionId connectionCallback = mapStore (HM.insert connectionId  c)
  where
    c = ClientConnection { connectionId , connectionCallback, connectionSessions = HM.empty }

adjust 
  :: (ClientConnection e m -> ClientConnection e m ) 
  -> ID
  -> StoreMap e m
adjust f key = mapStore (HM.adjust f key)

delete 
  :: ID
  -> StoreMap e m
delete key = mapStore (HM.delete key)