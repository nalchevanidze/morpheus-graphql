{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Server.Servant (servantServer) where

import Data.Morpheus.Server
  ( httpPubApp,
    webSocketsApp,
  )
import Network.Wai.Handler.Warp (run)
import Servant
  ( (:<|>) (..),
    Proxy (..),
    Server,
    serve,
  )
import Server.API.Simple
  ( EVENT,
    app,
  )
import Server.Utils
  ( Endpoint,
    serveEndpoint,
    servePubEndpoint,
    startServer,
  )

-- Server
type API =
  Endpoint "gql"
    :<|> Endpoint "mythology"

proxyApi :: Proxy API
proxyApi = Proxy

handler :: (EVENT -> IO ()) -> Server API
handler publish =
  servePubEndpoint app publish
    :<|> serveEndpoint app

servantServer :: IO ()
servantServer = do
  (wsApp, publish) <- webSocketsApp app
  startServer wsApp proxyApi (handler publish)
