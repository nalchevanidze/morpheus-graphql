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
  , statefull
  , storePublisher
  )
where

import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Network.WebSockets             ( ServerApp
                                                , Connection
                                                , sendTextData
                                                , receiveData
                                                )
import qualified Network.WebSockets          as WS

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLChannel(..) )
import           Data.Morpheus.Types.IO         ( MapAPI(..) )
import           Data.Morpheus.Types.Internal.Subscription
                                                ( connect
                                                , publish
                                                , disconnect
                                                , Input(..)
                                                , Stream
                                                , Store(..)
                                                , Scope(..)
                                                , API(..)
                                                , runStreamHTTP
                                                , runStreamWS
                                                , acceptApolloRequest
                                                )


storePublisher :: 
  ( MonadIO m
  , (Eq (StreamChannel event)) 
  , (GQLChannel event) 
  ) => Store event m -> event -> m ()
storePublisher store event = readStore store >>= publish event


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
    ( runStreamHTTP HTTP { httpCallback }
    . api 
    . Request
    )

defaultWSScope :: MonadIO m => Store e m -> Connection -> Scope 'Ws e m
defaultWSScope Store { writeStore } connection = WS 
  { listener = liftIO (receiveData connection)
  , callback = liftIO . sendTextData connection
  , update = writeStore
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
  let scope = defaultWSScope store connection
  pingThread connection $ do
      input <- connect 
      finally
        (handler scope input) 
        $ f $ disconnect scope input
 where
  handler scope input
        = f
        $ forever
        $ runStreamWS scope 
        $ streamApp input

-- | Same as above but specific to IO
gqlSocketApp
  :: (Input 'Ws -> Stream 'Ws e IO)
  -> Store e IO
  -> ServerApp
gqlSocketApp = gqlSocketMonadIOApp id