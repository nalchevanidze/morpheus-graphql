{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Data.Morpheus.Execution.Server.Resolve
  ( statelessResolver
  , RootResCon
  , fullSchema
  , coreResolver
  , EventCon
  )
where

import           Data.Aeson.Internal            ( formatError
                                                , ifromJSON
                                                )
import           Data.Aeson.Parser              ( eitherDecodeWith
                                                , jsonNoDup
                                                )
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Functor.Identity          ( Identity(..) )
import           Data.Proxy                     ( Proxy(..) )

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Encode
                                                ( EncodeCon
                                                , deriveModel
                                                )
import           Data.Morpheus.Execution.Server.Introspect
                                                ( IntroCon
                                                , introspectObjectFields
                                                , TypeScope(..)
                                                )
import           Data.Morpheus.Parsing.Request.Parser
                                                ( parseGQL )
import           Data.Morpheus.Schema.SchemaAPI ( defaultTypes
                                                , hiddenRootFields
                                                , schemaAPI
                                                )
import           Data.Morpheus.Types.GQLType    ( GQLType(CUSTOM) )
import           Data.Morpheus.Types.Internal.AST
                                                ( Operation(..)
                                                , DataFingerprint(..)
                                                , TypeContent(..)
                                                , Schema(..)
                                                , TypeDefinition(..)
                                                , MUTATION
                                                , QUERY
                                                , SUBSCRIPTION
                                                , initTypeLib
                                                , ValidValue
                                                , Name
                                                , VALIDATION_MODE(..)
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , FieldsDefinition(..)
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Merge(..)
                                                , empty
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLRootResolver(..)
                                                , Resolver
                                                , GQLChannel(..)
                                                , ResponseStream
                                                , Eventless
                                                , cleanEvents
                                                , ResultT(..)
                                                , Failure(..)
                                                , resolveUpdates
                                                , Context(..)
                                                , runResolverModel
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..)
                                                , GQLResponse(..)
                                                , renderResponse
                                                )
import           Data.Morpheus.Validation.Query.Validation
                                                ( validateRequest )
import           Data.Typeable                  ( Typeable )


type EventCon event
  = (Eq (StreamChannel event), Typeable event, GQLChannel event)

type IntrospectConstraint m event query mutation subscription
  = ( IntroCon (query (Resolver QUERY event m))
    , IntroCon (mutation (Resolver MUTATION event m))
    , IntroCon (subscription (Resolver SUBSCRIPTION event m))
    )

type RootResCon m event query mutation subscription
  = ( EventCon event
    , Typeable m
    , IntrospectConstraint m event query mutation subscription
    , EncodeCon QUERY event m (query (Resolver QUERY event m))
    , EncodeCon MUTATION event m (mutation (Resolver MUTATION event m))
    , EncodeCon
        SUBSCRIPTION
        event
        m
        (subscription (Resolver SUBSCRIPTION event m))
    )

statelessResolver
  :: (Monad m, RootResCon m event query mut sub)
  => GQLRootResolver m event query mut sub
  -> GQLRequest
  -> m GQLResponse
statelessResolver root req =
  renderResponse <$> runResultT (coreResolver root req)

coreResolver
  :: forall event m query mut sub
   . (Monad m, RootResCon m event query mut sub)
  => GQLRootResolver m event query mut sub
  -> GQLRequest
  -> ResponseStream event m ValidValue
coreResolver root request
  = validRequest >>=  execOperator
 where
  validRequest
    :: Monad m => ResponseStream event m Context
  validRequest = cleanEvents $ ResultT $ pure $ do
    schema     <- fullSchema $ Identity root
    operation  <- parseGQL request >>= validateRequest schema FULL_VALIDATION
    pure $ Context {
        schema
      , operation
      , currentTypeName = "Root"
      , currentSelection = Selection
          { selectionName = "Root"
          , selectionArguments = empty
          , selectionPosition = operationPosition operation
          , selectionAlias = Nothing
          , selectionContent = SelectionSet (operationSelection operation)
        }
  }
  ----------------------------------------------------------
  execOperator ctx@Context {schema } = runResolverModel (deriveModel root (schemaAPI schema)) ctx

fullSchema
  :: forall proxy m event query mutation subscription
   . (IntrospectConstraint m event query mutation subscription)
  => proxy (GQLRootResolver m event query mutation subscription)
  -> Eventless Schema
fullSchema _ = querySchema >>= mutationSchema >>= subscriptionSchema
 where
  querySchema = do
    fs <- hiddenRootFields <:> fields
    resolveUpdates (initTypeLib (operatorType fs "Query")) (defaultTypes : types)
   where
    (fields, types) = introspectObjectFields
      (Proxy @(CUSTOM (query (Resolver QUERY event m))))
      ("type for query", OutputType, Proxy @(query (Resolver QUERY event m)))
  ------------------------------
  mutationSchema lib = resolveUpdates
    (lib { mutation = maybeOperator fields "Mutation" })
    types
   where
    (fields, types) = introspectObjectFields
      (Proxy @(CUSTOM (mutation (Resolver MUTATION event m))))
      ( "type for mutation"
      , OutputType
      , Proxy @(mutation (Resolver MUTATION event m))
      )
  ------------------------------
  subscriptionSchema lib = resolveUpdates
    (lib { subscription = maybeOperator fields "Subscription" })
    types
   where
    (fields, types) = introspectObjectFields
      (Proxy @(CUSTOM (subscription (Resolver SUBSCRIPTION event m))))
      ( "type for subscription"
      , OutputType
      , Proxy @(subscription (Resolver SUBSCRIPTION event m))
      )
  maybeOperator :: FieldsDefinition -> Name -> Maybe TypeDefinition
  maybeOperator (FieldsDefinition x) | null x     = const Nothing
  maybeOperator fields = Just . operatorType fields
  -------------------------------------------------
  operatorType :: FieldsDefinition -> Name -> TypeDefinition
  operatorType fields typeName = TypeDefinition 
      { typeContent     = DataObject [] fields
        , typeName
        , typeFingerprint = DataFingerprint typeName []
        , typeMeta        = Nothing
      }