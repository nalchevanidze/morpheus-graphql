{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NamedFieldPuns         #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , gqlSocketMonadIOApp
  , initGQLState
  , Store
  , statefull
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
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                (empty)
import           Data.Morpheus.Types.Internal.Apollo
                                                ( acceptApolloRequest )
import           Data.Morpheus.Execution.Server.Subscription
                                                ( connect
                                                , disconnect
                                                , Action(..)
                                                , PubSubStore
                                                , Input(..)
                                                , Stream(..)
                                                , Scope(..)
                                                )
import           Data.Morpheus.Types.IO         ( MapAPI(..)
                                                , GQLResponse
                                                , renderResponse
                                                )

-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
newtype Store e m = Store {
  runStore :: Action e m -> m ()
}

run :: MonadIO m => WSStore e m -> Action e m -> m ()
run state (Update changes)  
    = modifyState_ state changes
run state (Notify runNotify)  
    = readState state 
      >>= runNotify

runStream 
  :: (Monad m) 
  => Store e m
  -> Scope e m
  -> Stream e m 
  ->  m GQLResponse
runStream store scope Stream { stream }  
  = do
    x <- runResultT (stream scope)
    renderResponse <$> 
      case x of 
        Success  r w events-> do 
          traverse_ (runStore store) events
          pure (Success r w []) 
        Failure x -> pure $ Failure x 

-- | initializes empty GraphQL state
initGQLState :: (MonadIO m) => IO (Store e m)
initGQLState = Store . run . WSStore <$> newMVar empty

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
  => Store e m
  -> (Input -> Stream e m)
  -> a
  -> m a
statefull store api 
  = mapAPI 
    ( runStream store HTTP { httpCallback = const $ pure ()}
    . api 
    . Request
    )

defaultWSScope :: MonadIO m => Connection -> Scope e m
defaultWSScope connection = WS 
  { listener = listen connection
  , callback = notify connection
  }

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (MonadIO m)
  => (m () -> IO ())
  -> (Input -> Stream e m)
  -> Store e m
  -> ServerApp
gqlSocketMonadIOApp f streamApp store pending = do
  connection <- acceptApolloRequest pending
  let scope = defaultWSScope connection
  pingThread connection $ do
      action <- connect 
      finally
        (handler scope action) 
        $ f $ traverse_ (runStore store) (disconnect action)
 where
  handler scope inputAction
        = f
        $ forever
        $ runStream store scope 
        $ streamApp inputAction

-- | Same as above but specific to IO
gqlSocketApp
  :: (Input -> Stream e IO)
  -> Store e IO
  -> ServerApp
gqlSocketApp = gqlSocketMonadIOApp id