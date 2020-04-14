{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |  GraphQL Wai Server Applications
module Data.Morpheus.Server
  ( gqlSocketApp
  , gqlSocketMonadIOApp
  , initGQLState
  , GQLState
  )
where


import           Control.Exception              ( finally )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Network.WebSockets             ( ServerApp
                                                , acceptRequestWith
                                                , pendingRequest
                                                , receiveData
                                                , withPingThread
                                                )

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Resolve
                                                ( RootResCon
                                                , coreResolver
                                                )
import           Data.Morpheus.Types.Internal.Apollo
                                                ( SubAction(..)
                                                , acceptApolloSubProtocol
                                                , apolloFormat
                                                )
import           Data.Morpheus.Execution.Server.Subscription
                                                ( GQLState
                                                , connectClient
                                                , disconnectClient
                                                , initGQLState
                                                , endSubscription
                                                , Stream(..)
                                                , Action(..)
                                                , handleSubscription
                                                , runStream
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..) )
import           Data.Morpheus.Types.Internal.WebSocket
                                                ( GQLClient(..) )

apolloToAction 
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub 
  -> GQLClient m e 
  -> SubAction  
  -> m (Stream m e) 
apolloToAction _ _ (SubError x) = pure $ Stream [Log x]
apolloToAction root client (AddSub sessionId request) 
  = handleSubscription client sessionId (coreResolver root request)
apolloToAction _ client (RemoveSub sessionId) 
  = pure $ endSubscription (clientID client) sessionId 

subscriptionHandler 
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub 
  -> GQLClient m e 
  -> m (Stream m e) 
subscriptionHandler root client = do
      d <- liftIO $ receiveData (clientConnection client)
      apolloToAction root client (apolloFormat d)

-- | Wai WebSocket Server App for GraphQL subscriptions
gqlSocketMonadIOApp
  :: (RootResCon m e que mut sub, MonadIO m)
  => GQLRootResolver m e que mut sub
  -> GQLState m e
  -> (m () -> IO ())
  -> ServerApp
gqlSocketMonadIOApp root state f pending = do
  connection <- acceptRequestWith pending
    $ acceptApolloSubProtocol (pendingRequest pending)
  withPingThread connection 30 (return ()) $ do
      (initStrem,client) <- connectClient connection
      f (runStream initStrem state)
      finally (queryHandler client) ( f $ runStream (disconnectClient client) state)
 where
  queryHandler client 
        = f
        $ forever
        $ subscriptionHandler root client 
          >>= (`runStream` state)

-- | Same as above but specific to IO
gqlSocketApp
  :: (RootResCon IO e que mut sub)
  => GQLRootResolver IO e que mut sub
  -> GQLState IO e
  -> ServerApp
gqlSocketApp gqlRoot state = gqlSocketMonadIOApp gqlRoot state id
