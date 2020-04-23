{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Morpheus.Types.Internal.Subscription.ClientStore
  ( Client(..)
  , ID
  , ClientStore
  , SesionID
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

data Client e ( m :: * -> * ) =
  Client
    { clientId       :: ID
    , clientCallback :: ByteString -> m ()
    -- one client can have multiple subsciprion session
    , clientSessions :: HashMap SesionID (SubEvent e m)
    }

instance Show (Client e m) where
  show Client { clientId, clientSessions } =
    "Client { id: "
      <> show clientId
      <> ", sessions: "
      <> show (keys clientSessions)
      <> " }"

publish
  :: ( Eq (StreamChannel event)
     , GQLChannel event
     , Monad m
     ) 
  => event 
  -> ClientStore event m 
  -> m ()
publish event = traverse_ sendMessage . elems
 where
  sendMessage Client { clientSessions, clientCallback }
    | null clientSessions  = pure ()
    | otherwise = traverse_ send (filterByChannels clientSessions)
   where
    send (sid, Event { content = subscriptionRes }) 
      = toApolloResponse sid <$> subscriptionRes event >>= clientCallback
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
newtype ClientStore e ( m :: * -> * ) = 
    ClientStore 
      { unpackStore :: HashMap ID (Client e m)
      } deriving (Show)

type StoreMap e m
  = ClientStore e m 
  -> ClientStore e m

mapStore 
  ::  ( HashMap ID (Client e m) 
      -> HashMap ID (Client e m)
      )
  -> StoreMap e m
mapStore f = ClientStore . f . unpackStore

elems :: ClientStore e m ->  [Client e m]
elems = HM.elems . unpackStore

instance Empty (ClientStore e m) where
  empty = ClientStore HM.empty

insert 
  :: ID
  -> (ByteString -> m ())
  -> StoreMap e m
insert clientId clientCallback = mapStore (HM.insert clientId  c)
  where
    c = Client { clientId , clientCallback, clientSessions = HM.empty }

adjust 
  :: (Client e m -> Client e m ) 
  -> ID
  -> StoreMap e m
adjust f key = mapStore (HM.adjust f key)

delete 
  :: ID
  -> StoreMap e m
delete key = mapStore (HM.delete key)