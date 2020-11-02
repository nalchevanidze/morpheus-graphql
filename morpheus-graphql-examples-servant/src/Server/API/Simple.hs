{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Server.API.Simple
  ( app,
    EVENT,
  )
where

import Data.Morpheus
  ( App,
    deriveApp,
  )
import Data.Morpheus.Document
  ( importGQLDocument,
  )
import Data.Morpheus.Subscriptions
  ( Event (..),
  )
import Data.Morpheus.Types
  ( ResolverM,
    RootResolver (..),
    publish,
    subscribe,
  )
import Data.Text (Text)

importGQLDocument "src/Server/API/simple.gql"

type EVENT = Event Label Contet

data Label
  = Update
  | New
  deriving (Eq, Show)

data Contet = Contet
  { deityName :: Text,
    deityPower :: Maybe Text
  }

rootResolver :: RootResolver IO EVENT Query Mutation Subscription
rootResolver =
  RootResolver
    { queryResolver = Query {deity},
      mutationResolver =
        Mutation
          { createDeity = resolveCreateDeity
          },
      subscriptionResolver =
        Subscription
          { newDeity
          }
    }
  where
    newDeity = subscribe New $ pure handler
      where
        handler (Event _ Contet {deityName, deityPower}) =
          pure $
            Deity
              { name = pure deityName,
                power = pure deityPower
              }
    deity DeityArgs {name} =
      pure
        Deity
          { name = pure name,
            power = pure (Just "Shapeshifting \n ")
          }

resolveCreateDeity :: CreateDeityArgs -> ResolverM EVENT IO Deity
resolveCreateDeity CreateDeityArgs {name, power} = do
  publish [Event [New] Contet {deityName = name, deityPower = power}]
  pure
    Deity
      { name = pure name,
        power = pure power
      }

app :: App EVENT IO
app = deriveApp rootResolver
