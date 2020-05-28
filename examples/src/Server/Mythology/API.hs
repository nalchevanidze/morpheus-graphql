{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Server.Mythology.API
  ( mythologyApi,
    mythologyRoot,
  )
where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Types
  ( GQLType,
    ResolverQ,
    RootResolver (..),
    Undefined (..),
    liftEither,
  )
import Data.Morpheus.Types.Directive (FieldDirective (..), ResolverDirective (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import GHC.TypeLits
import Server.Mythology.Character
  ( Deity (..),
    Human,
    dbDeity,
    someDeity,
    someHuman,
  )
import Server.Mythology.Place (City (..), Realm (..))

data Character m
  = CharacterHuman (Human m) -- Only <tyconName><conName> should generate direct link
  | CharacterDeity Deity -- Only <tyconName><conName> should generate direct link
      -- RECORDS
  | Creature {name :: Text, age :: Int}
  | BoxedDeity {boxedDeity :: Deity}
  | SomeScalarRecord {scalar :: Text}
  | --- Types
    SomeDeity Deity
  | SomeScalar Int
  | SomeMutli Int Text
  | --- ENUMS
    Zeus
  | Cronus
  deriving (Generic, GQLType)

data Arg (name :: Symbol) (value :: Symbol)

data Dir (name :: Symbol) (args :: [*])

type REST (url :: Symbol) = Dir "REST" '[Arg "url" url]

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    character :: [Character m],
    restDeity :: m (FieldDirective (REST "http/some-url") Deity)
  }
  deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name :: Text, -- Required Argument
    bornPlace :: Maybe City -- Optional Argument
  }
  deriving (Generic)

resolveDeity :: DeityArgs -> ResolverQ e IO Deity
resolveDeity DeityArgs {name, bornPlace} =
  liftEither $ dbDeity name bornPlace

instance (KnownSymbol a, Monad m) => ResolverDirective (REST a) m Deity where
  resolverDirective _ = case symbolVal (Proxy :: Proxy a) of
    "so" ->
      pure
        Deity
          { name = "so",
            power = Just "so",
            realm = Sky,
            bornAt = Nothing
          }
    val ->
      pure
        Deity
          { name = pack val,
            power = Just "",
            realm = Sky,
            bornAt = Nothing
          }

resolveCharacter :: Applicative m => [Character m]
resolveCharacter =
  [ CharacterHuman someHuman,
    CharacterDeity someDeity,
    Creature {name = "Lamia", age = 205},
    BoxedDeity {boxedDeity = someDeity},
    SomeScalarRecord {scalar = "Some Text"},
    ---
    SomeDeity someDeity,
    SomeScalar 12,
    SomeMutli 21 "some text",
    Zeus,
    Cronus
  ]

mythologyRoot :: RootResolver IO () Query Undefined Undefined
mythologyRoot =
  RootResolver
    { queryResolver =
        Query
          { deity = resolveDeity,
            character = resolveCharacter,
            restDeity = pure FieldDirective
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter mythologyRoot
