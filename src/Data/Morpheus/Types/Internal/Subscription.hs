{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Data.Morpheus.Types.Internal.Subscription
  ( connectionThread
  , toOutStream
  , runStreamWS
  , runStreamHTTP
  , Stream
  , Scope(..)
  , Input(..)
  , WS
  , HTTP
  , acceptApolloRequest
  , publish
  , Store(..)
  , initDefaultStore
  , publishEventWith
  )
where

import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Control.Concurrent             ( readMVar
                                                , newMVar
                                                , modifyMVar_
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.IO.Unlift        ( MonadUnliftIO
                                                , withRunInIO
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Operation
                                                (empty)
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLChannel(..) )
import           Data.Morpheus.Types.Internal.Subscription.Apollo
                                                ( acceptApolloRequest )
import           Data.Morpheus.Types.Internal.Subscription.ClientConnectionStore
                                                ( delete 
                                                , publish
                                                , ClientConnectionStore
                                                )
import           Data.Morpheus.Types.Internal.Subscription.Stream
                                                ( toOutStream
                                                , runStreamWS
                                                , runStreamHTTP
                                                , Stream
                                                , Scope(..)
                                                , Input(..)
                                                , WS
                                                , HTTP
                                                )
 
connect :: MonadIO m => m (Input WS)
connect = Init <$> liftIO nextRandom

disconnect:: Scope WS e m -> Input WS -> m ()
disconnect ScopeWS { update }  (Init clientID)  = update (delete clientID)

-- | PubSubStore interface
-- shared GraphQL state between __websocket__ and __http__ server,
-- you can define your own store if you provide write and read methods
-- to work properly Morpheus needs all entries of ClientConnectionStore (+ client Callbacks)
-- that why it is recomended that you use many local ClientStores on evenry server node
-- rathen then single centralized Store.
-- 
data Store e m = Store 
  { readStore :: m (ClientConnectionStore e m)
  , writeStore :: (ClientConnectionStore e m -> ClientConnectionStore e m) -> m ()
  }

publishEventWith :: 
  ( MonadIO m
  , (Eq (StreamChannel event)) 
  , (GQLChannel event) 
  ) => Store event m -> event -> m ()
publishEventWith store event = readStore store >>= publish event

-- | initializes empty GraphQL state
initDefaultStore :: 
  ( MonadIO m
  , MonadIO m2
  , (Eq (StreamChannel event)) 
  , (GQLChannel event) 
  ) 
  => m2 (Store event m)
initDefaultStore = do
  store <- liftIO $ newMVar empty
  pure Store 
    { readStore = liftIO $ readMVar store
    , writeStore = \changes -> liftIO $ modifyMVar_ store (return . changes)
    }


finallyM :: MonadUnliftIO m => m () -> m () -> m ()
finallyM loop end = withRunInIO $ \runIO -> finally (runIO loop) (runIO end)


connectionThread 
  :: ( MonadUnliftIO m
     ) 
  => (Input WS -> Stream WS e m) 
  -> Scope WS e m
  -> m ()
connectionThread api scope = do
  input <- connect 
  finallyM
    (connectionLoop api scope input)
    (disconnect scope input)

connectionLoop 
  :: Monad m 
  => (Input WS -> Stream WS e m) 
  -> Scope WS e m 
  -> Input WS 
  -> m ()
connectionLoop api scope input
            = forever
            $ runStreamWS scope 
            $ api input

