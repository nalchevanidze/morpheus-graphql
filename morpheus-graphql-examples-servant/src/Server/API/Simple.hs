{-# LANGUAGE DeriveAnyClass #-}
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

import Data.FileEmbed (makeRelativeToProject)
import Data.Morpheus
  ( App,
    deriveApp,
  )
import Data.Morpheus.Document
  ( importGQLDocument,
  )
import Data.Morpheus.Subscriptions
  ( Event (..),
    Hashable,
  )
import Data.Morpheus.Types
  ( ResolverM,
    RootResolver (..),
    publish,
    subscribe,
  )
import Data.Text (Text)
import GHC.Generics (Generic)

makeRelativeToProject "src/Server/API/simple.gql" >>= importGQLDocument

type EVENT = Event Channel Contet

data Channel
  = Update
  | New
  deriving
    ( Eq,
      Show,
      Generic,
      Hashable
    )

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
