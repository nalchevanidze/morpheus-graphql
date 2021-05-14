{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.JSONSchema.Parse
  ( decodeIntrospection,
  )
where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Morpheus.Client.JSONSchema.TypeKind (TypeKind (..))
import qualified Data.Morpheus.Client.JSONSchema.TypeRef as Ref
import Data.Morpheus.Client.JSONSchema.Types
  ( EnumValue (..),
    Field (..),
    InputValue (..),
    Introspection (..),
    JSONResponse (..),
    Schema (..),
    Type (..),
  )
import Data.Morpheus.Core
  ( defaultConfig,
    validateSchema,
  )
import Data.Morpheus.Error (globalErrorMessage)
import Data.Morpheus.Internal.Ext
  ( Eventless,
  )
import Data.Morpheus.Internal.Utils
  ( Failure (..),
    fromElems,
  )
import qualified Data.Morpheus.Types.Internal.AST as AST
  ( Schema,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition (..),
    CONST,
    FieldDefinition,
    IN,
    Message,
    OUT,
    OperationType (..),
    RootOperationTypeDefinition (..),
    SchemaDefinition (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    TypeWrapper (..),
    VALID,
    ValidationErrors,
    buildSchema,
    createScalarType,
    mkEnumContent,
    mkField,
    mkMaybeType,
    mkObjectField,
    mkType,
    mkUnionContent,
    msg,
    toAny,
  )
import Relude hiding
  ( ByteString,
    Type,
    fromList,
    show,
  )
import Prelude (show)

decoderError :: Message -> Eventless a
decoderError = failure . globalErrorMessage

decodeIntrospection :: ByteString -> Eventless (AST.Schema VALID)
decodeIntrospection jsonDoc = case jsonSchema of
  Left errors -> decoderError $ msg errors
  Right
    JSONResponse
      { responseData =
          Just
            Introspection
              { __schema =
                  schema@Schema {types}
              }
      } -> do
      schemaDef <- mkSchemaDef schema
      gqlTypes <- concat <$> traverse parse types
      buildSchema (Just schemaDef, gqlTypes, empty) >>= validate
  Right res -> decoderError (msg $ show res)
  where
    validate :: AST.Schema CONST -> Eventless (AST.Schema VALID)
    validate = validateSchema False defaultConfig
    jsonSchema :: Either String (JSONResponse Introspection)
    jsonSchema = eitherDecode jsonDoc

mkSchemaDef ::
  (Monad m, Failure ValidationErrors m) =>
  Schema ->
  m SchemaDefinition
mkSchemaDef
  Schema
    { queryType,
      mutationType,
      subscriptionType
    } =
    SchemaDefinition empty
      <$> fromElems
        ( catMaybes
            [ Just (RootOperationTypeDefinition Query $ Ref.name queryType),
              RootOperationTypeDefinition Mutation . Ref.name <$> mutationType,
              RootOperationTypeDefinition Subscription . Ref.name <$> subscriptionType
            ]
        )

class ParseJSONSchema a b where
  parse :: a -> Eventless b

instance ParseJSONSchema Type [TypeDefinition ANY CONST] where
  parse Type {name = Just typeName, kind = SCALAR} =
    pure [createScalarType typeName]
  parse Type {name = Just typeName, kind = ENUM, enumValues = Just enums} =
    pure [mkType typeName $ mkEnumContent (fmap enumName enums)]
  parse Type {name = Just typeName, kind = UNION, possibleTypes = Just unions} =
    case traverse name unions of
      Nothing -> decoderError "ERROR: GQL ERROR"
      Just uni -> pure [toAny $ mkType typeName $ mkUnionContent uni]
  parse Type {name = Just typeName, kind = INPUT_OBJECT, inputFields = Just iFields} =
    do
      (fields :: [FieldDefinition IN CONST]) <- traverse parse iFields
      fs <- fromElems fields
      pure [mkType typeName $ DataInputObject fs]
  parse Type {name = Just typeName, kind = OBJECT, fields = Just oFields} =
    do
      (fields :: [FieldDefinition OUT CONST]) <- traverse parse oFields
      fs <- fromElems fields
      pure [mkType typeName $ DataObject [] fs]
  parse _ = pure []

instance ParseJSONSchema Field (FieldDefinition OUT CONST) where
  parse Field {fieldName, fieldArgs, fieldType} = do
    TypeRef typename wrappers <- fieldTypeFromJSON fieldType
    args <- traverse genArg fieldArgs >>= fromElems
    pure $ mkObjectField args fieldName wrappers typename
    where
      genArg InputValue {inputName = argName, inputType = argType} =
        ArgumentDefinition . mkField Nothing argName <$> fieldTypeFromJSON argType

instance ParseJSONSchema InputValue (FieldDefinition IN CONST) where
  parse InputValue {inputName, inputType} = mkField Nothing inputName <$> fieldTypeFromJSON inputType

fieldTypeFromJSON :: Type -> Eventless TypeRef
fieldTypeFromJSON Type {kind = NON_NULL, ofType = Just ofType} = withListNonNull <$> fieldTypeFromJSON ofType
fieldTypeFromJSON Type {kind = LIST, ofType = Just ofType} = withList <$> fieldTypeFromJSON ofType
fieldTypeFromJSON Type {name = Just name} = pure (TypeRef name mkMaybeType)
fieldTypeFromJSON x = decoderError $ "Unsupported Field" <> msg (show x)

withList :: TypeRef -> TypeRef
withList (TypeRef name x) = TypeRef name (TypeList x False)

withListNonNull :: TypeRef -> TypeRef
withListNonNull (TypeRef name (TypeList y _)) = TypeRef name (TypeList y True)
withListNonNull (TypeRef name (BaseType _)) = TypeRef name (BaseType True)
