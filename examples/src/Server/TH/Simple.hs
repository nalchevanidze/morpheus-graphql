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

module Server.TH.Simple
  ( thSimpleApi,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined (..))
import Data.Morpheus.Types.Directive (ResolverDirective (..))
import Data.Text (Text)

importGQLDocument "src/Server/TH/simple.gql"

-- newtype Rest = Rest {url :: Text}

-- instance
--   Monad m =>
--   ResolverDirective Rest (Deity m)
--   where
--   fieldResolver _ Rest {url = "assets/deity-1"} = Deity {name = pure "", power = pure $ Just ""}
--   fieldResolver _ Rest {} = Deity {name = pure "", power = pure $ Just ""}

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

thSimpleApi :: ByteString -> IO ByteString
thSimpleApi = interpreter rootResolver
