{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE DataKinds             #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , gqlSocketMonadIOApp
  , initGQLState
  , Store(..)
  , statefull
  , storePublisher
  )
where

import           Control.Monad                  ((>=>))
import           Data.ByteString.Lazy.Char8     (ByteString)
import           Data.Foldable                  ( traverse_ )
import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Network.WebSockets             ( ServerApp
                                                , Connection
                                                , sendTextData
                                                , receiveData
                                                )
import qualified Network.WebSockets          as WS
import           Control.Concurrent             ( MVar
                                                , readMVar
                                                , newMVar
                                                , modifyMVar_
                                                )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Resolving
                                                ( ResultT(..)
                                                , Result(..)
                                                , GQLChannel(..)
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                (empty)
import           Data.Morpheus.Types.Internal.Apollo
                                                ( acceptApolloRequest )
import           Data.Morpheus.Types.Internal.Subscription
                                                (publish)
import           Data.Morpheus.Execution.Server.Subscription
                                                ( connect
                                                , disconnect
                                                , Action(..)
                                                , PubSubStore
                                                , Input(..)
                                                , Stream(..)
                                                , Scope(..)
                                                , API(..)
                                                )
import           Data.Morpheus.Types.IO         ( MapAPI(..)
                                                , GQLResponse
                                                , renderResponse
                                                )

-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
data Store e m = Store 
  { readStore :: m (PubSubStore e m)
  , writeStore :: (PubSubStore e m -> PubSubStore e m) -> m ()
  }

run :: Store e m -> Action e m -> m ()
run state (Update changes)  
    = writeStore state changes

runStream 
  :: (Monad m) 
  => Store e m
  -> Scope api e m
  -> Stream api e m 
  ->  m GQLResponse
runStream store scope StreamWS { streamWS }  
  = do
    x <- runResultT (streamWS scope)
    renderResponse <$> 
      case x of 
        Success  r w events-> do 
          traverse_ (run store) events
          pure (Success r w []) 
        Failure x -> pure $ Failure x 
runStream _ HTTP{ httpCallback } StreamHTTP { streamHTTP }  
  = do
    x <- runResultT (streamHTTP httpCallback)
    renderResponse <$>
      case x of 
        Success  r w events-> do 
          traverse_ httpCallback events
          pure (Success r w []) 
        Failure x -> pure $ Failure x 

-- | initializes empty GraphQL state
initGQLState :: 
  ( MonadIO m
  , (Eq (StreamChannel event)) 
  , (GQLChannel event) 
  ) 
  => IO (Store event m)
initGQLState = do
  store <- WSStore <$> newMVar empty
  pure Store 
    { readStore = readState store
    , writeStore = modifyState_ store
    }

storePublisher :: 
  ( MonadIO m
  , (Eq (StreamChannel event)) 
  , (GQLChannel event) 
  ) => Store event m -> event -> m ()
storePublisher store event = readStore store >>= publish event

newtype WSStore e m = WSStore { unWSStore :: MVar (PubSubStore e m) }

listen :: MonadIO m => Connection -> m ByteString
listen = liftIO . receiveData

notify :: MonadIO m => Connection -> ByteString -> m ()
notify conn = liftIO . sendTextData conn

readState :: (MonadIO m) => WSStore e m -> m (PubSubStore e m)
readState = liftIO . readMVar . unWSStore

modifyState_ 
  :: (MonadIO m) 
  => WSStore e m 
  -> (PubSubStore e m -> PubSubStore e m) 
  -> m ()
modifyState_ state changes = liftIO $ modifyMVar_ (unWSStore state) (return . changes)


-- support old version of Websockets
pingThread :: Connection -> IO () -> IO ()
#if MIN_VERSION_websockets(0,12,6)
pingThread connection = WS.withPingThread connection 30 (return ())
#else
pingThread connection = (WS.forkPingThread connection 30 >>)
#endif

statefull
  ::  
   ( MonadIO m,
     MapAPI a
   )
  => (e -> m ())
  -> (Input 'Http -> Stream 'Http e m)
  -> a
  -> m a
statefull httpCallback api 
  = mapAPI 
    ( runStream undefined HTTP { httpCallback }
    . api 
    . Request
    )

defaultWSScope :: MonadIO m => Connection -> Scope 'Ws e m
defaultWSScope connection = WS 
  { listener = listen connection
  , callback = notify connection
  }

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (MonadIO m)
  => (m () -> IO ())
  -> (Input 'Ws -> Stream 'Ws e m)
  -> Store e m
  -> ServerApp
gqlSocketMonadIOApp f streamApp store pending = do
  connection <- acceptApolloRequest pending
  let scope = defaultWSScope connection
  pingThread connection $ do
      input <- connect 
      finally
        (handler scope input) 
        $ f $ run store (disconnect input)
 where
  handler scope input
        = f
        $ forever
        $ runStream store scope 
        $ streamApp input

-- | Same as above but specific to IO
gqlSocketApp
  :: (Input 'Ws -> Stream 'Ws e IO)
  -> Store e IO
  -> ServerApp
gqlSocketApp = gqlSocketMonadIOApp id