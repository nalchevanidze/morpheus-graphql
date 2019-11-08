{-# LANGUAGE DataKinds #-}
-- | GQL Types
module Data.Morpheus.Types
  ( Event(..)
  -- Type Classes
  , GQLType(KIND, description)
  , GQLScalar
    ( parseValue
    , serialize
  -- Values
    )
  , GQLRequest(..)
  , GQLResponse(..)
  , ID(..)
  , ScalarValue(..)
  , GQLRootResolver(..)
  , constRes
  , constMutRes
  , Undefined(..)
  , Res
  , MutRes
  , SubRes
  , IORes
  , IOMutRes
  , IOSubRes
  , Resolver(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , liftEither
  , lift
  )
where

import           Data.Morpheus.Types.GQLScalar  ( GQLScalar
                                                  ( parseValue
                                                  , serialize
                                                  )
                                                )
import           Data.Morpheus.Types.GQLType    ( GQLType(KIND, description) )
import           Data.Morpheus.Types.ID         ( ID(..) )
import           Data.Morpheus.Types.Internal.Data
                                                ( MUTATION
                                                , QUERY
                                                , SUBSCRIPTION
                                                )
import           Data.Morpheus.Types.Internal.Resolver
                                                ( Event(..)
                                                , GQLRootResolver(..)
                                                , PureOperation
                                                , Resolver(..)
                                                , liftEither
                                                , lift
                                                )
import           Data.Morpheus.Types.Internal.Value
                                                ( ScalarValue(..) )
import           Data.Morpheus.Types.IO         ( GQLRequest(..)
                                                , GQLResponse(..)
                                                )
import           Data.Morpheus.Types.Types      ( Undefined(..) )

type Res = Resolver QUERY
type MutRes = Resolver MUTATION
type SubRes = Resolver SUBSCRIPTION

type IORes e = Res e IO
type IOMutRes e = MutRes e IO
type IOSubRes e = SubRes e IO

-- resolves constant value on any argument
constRes :: (PureOperation o, Monad m) => b -> a -> Resolver o e m b
constRes = const . pure

constMutRes :: Monad m => [e] -> a -> args -> MutRes e m a
constMutRes list value = const $ MutResolver list $ pure value
