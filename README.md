# Morpheus GraphQL [![Hackage](https://img.shields.io/hackage/v/morpheus-graphql.svg)](https://hackage.haskell.org/package/morpheus-graphql) ![CI](https://github.com/morpheusgraphql/morpheus-graphql/workflows/CI/badge.svg)

Build GraphQL APIs with your favorite functional language!

Morpheus GraphQL (Server & Client) helps you to build GraphQL APIs in Haskell with native Haskell types.
Morpheus will convert your Haskell types to a GraphQL schema and all your resolvers are just native Haskell functions. Morpheus GraphQL can also convert your GraphQL Schema or Query to Haskell types and validate them in compile time.

Morpheus is still in an early stage of development, so any feedback is more than welcome, and we appreciate any contribution!
Just open an issue here on GitHub, or join [our Slack channel](https://morpheus-graphql-slack-invite.herokuapp.com/) to get in touch.

## Getting Started

### Setup

To get started with Morpheus, you first need to add it to your project's dependencies, as follows (assuming you're using hpack):

_package.yml_

```yaml
dependencies:
  - morpheus-graphql
```

Additionally, you should tell stack which version to pick:

_stack.yml_

```yaml
resolver: lts-16.2

extra-deps:
  - morpheus-graphql-0.17.0
```

As Morpheus is quite new, make sure stack can find morpheus-graphql by running `stack upgrade` and `stack update`

### Building your first GraphQL API

### with GraphQL syntax

_schema.gql_

```gql
type Query {
  deity(name: String! = "Morpheus"): Deity!
}

"""
Description for Deity
"""
type Deity {
  """
  Description for name
  """
  name: String!
  power: String @deprecated(reason: "some reason for")
}
```

_API.hs_

```haskell
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

module API (api) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined (..))
import Data.Text (Text)

importGQLDocument "schema.gql"

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    deity DeityArgs {name} =
      pure
        Deity
          { name = pure name,
            power = pure (Just "Shapeshifting")
          }

api :: ByteString -> IO ByteString
api = interpreter rootResolver
```

Template Haskell Generates types: `Query` , `Deity`, `DeityArgs`, that can be used by `rootResolver`

`descriptions` and `deprecations` will be displayed in introspection.

`importGQLDocumentWithNamespace` will generate Types with namespaced fields. If you don't need namespace use `importGQLDocument`

### with Native Haskell Types

To define a GraphQL API with Morpheus we start by defining the API Schema as a native Haskell data type,
which derives the `Generic` typeClass. Lazily resolvable fields on this `Query` type are defined via `a -> ResolverQ () IO b`, representing resolving a set of arguments `a` to a concrete value `b`.

```haskell
data Query m = Query
  { deity :: DeityArgs -> m Deity
  } deriving (Generic, GQLType)

data Deity = Deity
  { fullName :: Text         -- Non-Nullable Field
  , power    :: Maybe Text   -- Nullable Field
  } deriving (Generic,GQLType)

data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  } deriving (Generic)
```

For each field in the `Query` type defined via `a -> m b` (like `deity`) we will define a resolver implementation that provides the values during runtime by referring to
some data source, e.g. a database or another API. Fields that are defined without `a -> m b` you can just provide a value.

In above example, the field of `DeityArgs` could also be named using reserved identities (such as: `type`, `where`, etc), in order to avoid conflict, a prime symbol (`'`) must be attached. For example, you can have:

```haskell
data DeityArgs = DeityArgs
  { name      :: Text        -- Required Argument
  , mythology :: Maybe Text  -- Optional Argument
  , type'     :: Text
  } deriving (Generic)
```

The field name in the final request will be `type` instead of `type'`. The Morpheus request parser converts each of the reserved identities in Haskell 2010 to their corresponding names internally. This also applies to selections.

```haskell
resolveDeity :: DeityArgs -> ResolverQ e () Deity
resolveDeity DeityArgs { name, mythology } = liftEither $ dbDeity name mythology

askDB :: Text -> Maybe Text -> IO (Either String Deity)
askDB = ...
```

To make this `Query` type available as an API, we define a `RootResolver` and feed it to the Morpheus `interpreter`. A `RootResolver` consists of `query`, `mutation` and `subscription` definitions, while we omit the latter for this example:

```haskell
rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {deity = resolveDeity}
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver
```

As you can see, the API is defined as `ByteString -> IO ByteString` which we can either invoke directly or use inside an arbitrary web framework
such as `scotty` or `serverless-haskell`. We'll go for `scotty` in this example:

```haskell
main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . gqlApi =<< body)
```

If we now send a POST request to `http://localhost:3000/api` with a GraphQL Query as body for example in a tool like `Insomnia`:

```GraphQL
query GetDeity {
  deity (name: "Morpheus") {
    fullName
    power
  }
}
```

our query will be resolved!

```JSON
{
  "data": {
    "deity": {
      "fullName": "Morpheus",
      "power": "Shapeshifting"
    }
  }
}
```

## Serverless Example

If you are interested in creating a `Morpheus GraphQL` API with `Serverless`, you should take a look at our example in this repository:
[_Mythology API_](https://github.com/morpheusgraphql/mythology-api) it is our example project build with `Morpheus GraphQL` and `Serverless-Haskell`,
where you can query different mythology characters with `GraphiQL`.

Mythology API is deployed on : [_api.morpheusgraphql.com_](https://api.morpheusgraphql.com) where you can test it with `GraphiQL`

![Mythology Api](https://morpheusgraphql.com/assets/img/mythology-api.png "mythology-api")

## Advanced topics

### Enums

You can use Union Types as Enums, but they're not allowed to have any parameters.

```haskell
data City
  = Athens
  | Sparta
  | Corinth
  | Delphi
  | Argos
  deriving (Generic)

instance GQLType City where
  type KIND City = ENUM
```

### Union types

To use union type, all you have to do is derive the `GQLType` class. Using GraphQL [_fragments_](https://graphql.org/learn/queries/#fragments), the arguments of each data constructor can be accessed from the GraphQL client.

```haskell
data Character
  = CharacterDeity Deity -- will be unwrapped, since Character + Deity = CharacterDeity
  | SomeDeity Deity -- will be wrapped since Character + Deity != SomeDeity
  | Creature { creatureName :: Text, creatureAge :: Int }
  | Demigod Text Text
  | Zeus
  deriving (Generic, GQLType)
```

where `Deity` is an object.

As we see, there are different kinds of unions. `Morpheus` handles them all.

This type will be represented as

```gql
union Character = Deity | SomeDeity | Creature | SomeMulti | Zeus

type SomeDeity {
  _0: Deity!
}

type Creature {
  creatureName: String!
  creatureAge: Int!
}

type Demigod {
  _0: Int!
  _1: String!
}

type Zeus {
  _: Unit!
}
```

By default, union members will be generated with wrapper objects.
There is one exception to this: if a constructor of a type is the type name concatenated with the name of the contained type, it will be referenced directly.
That is, given:

```haskell
data Song = { songName :: Text, songDuration :: Float } deriving (Generic, GQLType)

data Skit = { skitName :: Text, skitDuration :: Float } deriving (Generic, GQLType)

data WrappedNode
  = WrappedSong Song
  | WrappedSkit Skit
  deriving (Generic, GQLType)

data NonWrapped
  = NonWrappedSong Song
  | NonWrappedSkit Skit
  deriving (Generic, GQLType)

```

You will get the following schema:

```gql
# has wrapper types
union WrappedNode = WrappedSong | WrappedSkit

# is a direct union
union NonWrapped = Song | Skit

type WrappedSong {
  _0: Song!
}

type WrappedSKit {
  _0: Skit!
}

type Song {
  songDuration: Float!
  songName: String!
}

type Skit {
  skitDuration: Float!
  skitName: String!
}
```

- for all other unions will be generated new object type. for types without record syntax, fields will be automatically indexed.

- all empty constructors in union will be summed in type `<tyConName>Enum` (e.g `CharacterEnum`), this enum will be wrapped in `CharacterEnumObject` and added to union members.

### Scalar types

To use custom scalar types, you need to provide implementations for `parseValue` and `serialize` respectively.

```haskell
data Odd = Odd Int  deriving (Generic)

instance DecodeScalar Euro where
  decodeScalar (Int x) = pure $ Odd (... )
  decodeScalar _ = Left "invalid Value!"

instance EncodeScalar Euro where
  encodeScalar (Odd value) = Int value

instance GQLType Odd where
  type KIND Odd = SCALAR
```

### Applicative and Monad instance

The `Resolver` type has `Applicative` and `Monad` instances that can be used to compose resolvers.

### Introspection

Morpheus converts your schema to a GraphQL introspection automatically. You can use tools like `Insomnia` to take a
look at the introspection and validate your schema.
If you need a description for your GQLType inside of the introspection you can define the GQLType instance manually
and provide an implementation for the `description` function:

```haskell
data Deity = Deity
{ ...
} deriving (Generic)

instance GQLType Deity where
  description = const "A supernatural being considered divine and sacred"
```

screenshots from `Insomnia`

![alt text](https://morpheusgraphql.com/assets/img/introspection/spelling.png "spelling")
![alt text](https://morpheusgraphql.com/assets/img/introspection/autocomplete.png "autocomplete")
![alt text](https://morpheusgraphql.com/assets/img/introspection/type.png "type")

## Handling Errors

for errors you can use use either `liftEither` or `MonadFail`:
at the and they have same result.

with `liftEither`

```haskell
resolveDeity :: DeityArgs -> ResolverQ e IO Deity
resolveDeity DeityArgs {} = liftEither $ dbDeity

dbDeity ::  IO Either Deity
dbDeity = pure $ Left "db error"
```

with `MonadFail`

```haskell
resolveDeity :: DeityArgs -> ResolverQ e IO Deity
resolveDeity DeityArgs { } = fail "db error"
```

### Mutations

In addition to queries, Morpheus also supports mutations. They behave just like regular queries and are defined similarly:

```haskell
newtype Mutation m = Mutation
  { createDeity :: MutArgs -> m Deity
  } deriving (Generic, GQLType)

rootResolver :: RootResolver IO  () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {...}
    , mutationResolver = Mutation { createDeity }
    , subscriptionResolver = Undefined
    }
    where
      -- Mutation Without Event Triggering
      createDeity :: MutArgs -> ResolverM () IO Deity
      createDeity_args = lift setDBAddress

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter rootResolver
```

### Subscriptions

In morpheus subscription and mutation communicate with Events,
`Event` consists with user defined `Channel` and `Content`.

Every subscription has its own Channel by which it will be triggered

```haskell
data Channel
  = ChannelA
  | ChannelB

data Content
  = ContentA Int
  | ContentB Text

type MyEvent = Event Channel Content

newtype Query m = Query
  { deity :: m Deity
  } deriving (Generic)

newtype Mutation m = Mutation
  { createDeity :: m Deity
  } deriving (Generic)

newtype Subscription (m ::  * -> * ) = Subscription
  { newDeity :: m  Deity
  } deriving (Generic)

newtype Subscription (m :: * -> *) = Subscription
{ newDeity :: SubscriptionField (m Deity),
}
deriving (Generic)


type APIEvent = Event Channel Content

rootResolver :: RootResolver IO APIEvent Query Mutation Subscription
rootResolver = RootResolver
  { queryResolver        = Query { deity = fetchDeity }
  , mutationResolver     = Mutation { createDeity }
  , subscriptionResolver = Subscription { newDeity }
  }
 where
  -- Mutation Without Event Triggering
  createDeity :: ResolverM EVENT IO Address
  createDeity = do
      requireAuthorized
      publish [Event { channels = [ChannelA], content = ContentA 1 }]
      lift dbCreateDeity
  newDeity :: SubscriptionField (ResolverS EVENT IO Deity)
  newDeity = subscribe ChannelA $ do
    -- executed only once
    -- immediate response on failures
    requireAuthorized
    pure $ \(Event _ content) -> do
        -- executes on every event
        lift (getDBAddress content)
```

### Interface

1. defining interface with Haskell Types (runtime validation):

   ```haskell
     -- interface is just regular type derived as interface
   newtype Person m = Person {name ::  m Text}
     deriving (Generic)

   instance GQLType (Person m) where
     type KIND (Person m) = INTERFACE

   -- with GQLType user can links interfaces to implementing object
   instance GQLType Deity where
     implements _ = [interface (Proxy @Person)]
   ```

2. defining with `importGQLDocument` and `DSL` (compile time validation):

   ```graphql
   interface Account {
     name: String!
   }

   type User implements Account {
     name: String!
   }
   ```

## Morpheus `GraphQL Client` with Template haskell QuasiQuotes

```hs
defineByDocumentFile
    "./schema.gql"
  [gql|
    query GetHero ($character: Character)
      {
        deity (fatherOf:$character) {
          name
          power
          worships {
            deity2Name: name
          }
        }
      }
  |]
```

with schema:

```gql
input Character {
  name: String!
}

type Deity {
  name: String!
  worships: Deity
  power: Power!
}

enum Power {
  Lightning
  Teleportation
  Omniscience
}
```

will validate query and Generate:

- namespaced response and variable types
- instance for `Fetch` typeClass

```hs
data GetHero = GetHero {
  deity: DeityDeity
}

-- from: {user
data DeityDeity = DeityDeity {
  name: Text,
  worships: Maybe DeityWorshipsDeity
  power: Power
}

-- from: {deity{worships
data DeityWorshipsDeity = DeityWorshipsDeity {
  name: Text,
}

data Power =
    PowerLightning
  | PowerTeleportation
  | PowerOmniscience

data GetHeroArgs = GetHeroArgs {
  character: Character
}

data Character = Character {
  name: Person
}
```

as you see, response type field name collision can be handled with GraphQL `alias`.

with `fetch` you can fetch well typed response `GetHero`.

```haskell
  fetchHero :: Args GetHero -> m (Either FetchError (FetchResult GetHero))
  fetchHero = fetch jsonRes args
      where
        args = GetHeroArgs {character = Person {name = "Zeus"}}
        jsonRes :: ByteString -> m ByteString
        jsonRes = <GraphQL APi>
```

in this case, `jsonRes` resolves a request into a response in some monad `m`.

A `fetch` resolver implementation against [a real API](https://swapi.graph.cool) may look like the following:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Req

resolver :: String -> ByteString -> IO ByteString
resolver tok b = runReq defaultHttpConfig $ do
    let headers = header "Content-Type" "application/json"
    responseBody <$> req POST (https "swapi.graph.cool") (ReqBodyLbs b) lbsResponse headers
```

this is demonstrated in examples/src/Client/StarWarsClient.hs

types can be generated from `introspection` too:

```haskell
defineByIntrospectionFile "./introspection.json"
```

## Morpheus CLI for Code Generating

you should use [morpheus-graphql-cli](https://github.com/morpheusgraphql/morpheus-graphql-cli)

## Showcase

Below are the list of projects using Morpheus GraphQL. If you want to start using Morpheus GraphQL, they are
good templates to begin with.

- https://github.com/morpheusgraphql/mythology-api
  - Serverless Mythology API
- https://github.com/dandoh/web-haskell
  - Modern webserver boilerplate in Haskell: Morpheus Graphql + Postgresql + Authentication + DB migration + Dotenv and more

_Edit this section and send PR if you want to share your project_.

# About

## The name

_Morpheus_ is the greek god of sleep and dreams whose name comes from the greek word _μορφή_ meaning form or shape.
He is said to be able to mimic different forms and GraphQL is good at doing exactly that: Transforming data in the shape
of many different APIs.

## Team

Morpheus is written and maintained by [_nalchevanidze_](https://github.com/nalchevanidze)

## Roadmap

- Medium future:
  - Stabilize API
  - Specification-isomorphic error handling
- Long term:
  - Support all possible GQL features
  - Performance optimization
