{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Scotty
  ( scottyServer,
  )
where

-- examples
import Client.Client
  ( fetchUser,
  )
import Data.Functor.Identity (Identity (..))
import Data.Morpheus.Server
  ( compileTimeSchemaValidation,
    httpPubApp,
    webSocketsApp,
  )
import qualified Server.Mythology.API as Mythology
import Server.Sophisticated.API
  ( EVENT,
    app,
    root,
  )
import qualified Server.TH.Simple as TH
import Server.Utils
  ( httpEndpoint,
    httpPubEndpoint,
    startServer,
  )
import Web.Scotty
  ( ScottyM,
  )

_validateSchema :: ()
_validateSchema = $(compileTimeSchemaValidation (Identity root))

scottyServer :: IO ()
scottyServer = do
  (wsApp, publish) <- webSocketsApp app
  fetchUser (httpPubApp app publish) >>= print
  startServer wsApp (httpApp publish)
  where
    httpApp :: (EVENT -> IO ()) -> ScottyM ()
    httpApp publish = do
      httpEndpoint "/" httpPubEndpoint app publish
      httpEndpoint "/mythology" Mythology.app
      httpEndpoint "/th" TH.app
