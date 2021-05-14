{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Base
  ( Ref (..),
    Position (..),
    Message (..),
    FieldName (..),
    Description,
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    Token,
    isNotSystemTypeName,
    sysFields,
    hsTypeName,
    toOperationType,
    GQLError (..),
    GQLErrors,
    TRUE,
    FALSE,
    TypeName (..),
    Msg (..),
    intercalateName,
    toFieldName,
    convertToJSONName,
    convertToHaskellName,
    InternalError (..),
    msgInternal,
    ValidationError (..),
    msgValidation,
    ValidationErrors,
    withPosition,
    toGQLError,
    unitTypeName,
    unitFieldName,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value,
    encode,
  )
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    fromText,
    renderGQL,
  )
import Data.Text (intercalate, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Language.Haskell.TH
  ( ExpQ,
    stringE,
  )
import Language.Haskell.TH.Syntax
  ( Lift (..),
    Q,
    TExp,
    unsafeTExpCoerce,
  )
import Relude hiding
  ( ByteString,
    decodeUtf8,
    intercalate,
  )

type TRUE = 'True

type FALSE = 'False

-- Strings
type Token = Text

-- Error / Warning Messages
newtype Message = Message {readMessage :: Text}
  deriving
    (Generic)
  deriving newtype
    (Show, Eq, Ord, IsString, Semigroup, Hashable, FromJSON, ToJSON)

instance Lift Message where
  lift = liftString . readMessage

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . readMessage
#endif

newtype InternalError = InternalError
  { readInternalError :: Text
  }
  deriving
    (Generic)
  deriving newtype
    (Show, Eq, Ord, IsString, Semigroup, Hashable, FromJSON, ToJSON)

data ValidationError = ValidationError
  { validationMessage :: Message,
    validationLocations :: [Position]
  }
  deriving (Show)

instance IsString ValidationError where
  fromString = (`ValidationError` []) . msg

instance Semigroup ValidationError where
  ValidationError m1 p1 <> ValidationError m2 p2 =
    ValidationError (m1 <> m2) (p1 <> p2)

withPosition :: Maybe Position -> ValidationError -> ValidationError
withPosition pos (ValidationError m ps) = ValidationError m (ps <> maybeToList pos)
{-# INLINE withPosition #-}

type ValidationErrors = [ValidationError]

toGQLError :: ValidationError -> GQLError
toGQLError (ValidationError m p) = GQLError m p
{-# INLINE toGQLError #-}

-- instance Lift InternalError where
--   lift = liftString . readInternalError

-- #if MIN_VERSION_template_haskell(2,16,0)
--   liftTyped = liftTypedString . readInternalError
-- #endif

msgInternal :: (Msg a) => a -> InternalError
msgInternal = InternalError . readMessage . msg
{-# INLINE msgInternal #-}

msgValidation :: (Msg a) => a -> ValidationError
msgValidation = (`ValidationError` []) . msg
{-# INLINE msgValidation #-}

class Msg a where
  msg :: a -> Message
  msgSepBy :: Text -> [a] -> Message
  msgSepBy t = Message . intercalate t . fmap (readMessage . msg)

instance Msg Message where
  msg = id

instance Msg InternalError where
  msg = Message . ("Internal Error! " <>) . readInternalError

instance Msg String where
  msg = Message . pack

instance Msg ByteString where
  msg = Message . LT.toStrict . decodeUtf8

instance Msg Text where
  msg = Message

instance Msg Value where
  msg = msg . encode

-- FieldName : lower case names
newtype FieldName = FieldName {readName :: Text}
  deriving
    (Generic)
  deriving newtype
    (Show, Ord, Eq, IsString, Hashable, Semigroup, FromJSON, ToJSON)

instance Lift FieldName where
  lift = liftString . readName

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . readName
#endif

instance Msg FieldName where
  msg FieldName {readName} = Message $ "\"" <> readName <> "\""

instance RenderGQL FieldName where
  renderGQL = fromText . readName

intercalateName :: FieldName -> [FieldName] -> FieldName
intercalateName (FieldName x) = FieldName . intercalate x . fmap readName
{-# INLINE intercalateName #-}

toFieldName :: TypeName -> FieldName
toFieldName = FieldName . readTypeName
{-# INLINE toFieldName #-}

-- TypeName
newtype TypeName = TypeName {readTypeName :: Text}
  deriving
    (Generic)
  deriving newtype
    ( Show,
      Ord,
      Eq,
      IsString,
      Hashable,
      Semigroup,
      FromJSON,
      ToJSON
    )

instance Lift TypeName where
  lift = liftString . readTypeName

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped = liftTypedString . readTypeName
#endif

liftTypedString :: IsString a => Token -> Q (TExp a)
liftTypedString = unsafeTExpCoerce . stringE . T.unpack
{-# INLINE liftTypedString #-}

liftString :: Token -> ExpQ
liftString = stringE . T.unpack
{-# INLINE liftString #-}

instance Msg TypeName where
  msg TypeName {readTypeName} = Message $ "\"" <> readTypeName <> "\""

instance RenderGQL TypeName where
  renderGQL = fromText . readTypeName

-- Description
type Description = Text

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON, Lift)

instance Ord Position where
  compare x y = compare (line x) (line y)

-- Positions 2 Value with same structure
-- but different Positions should be Equal
instance Eq Position where
  _ == _ = True

data GQLError = GQLError
  { message :: Message,
    locations :: [Position]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

type GQLErrors = [GQLError]

data OperationType
  = Query
  | Subscription
  | Mutation
  deriving (Show, Eq, Lift, Generic, Hashable)

instance RenderGQL OperationType where
  renderGQL = fromString . fmap toLower . show

instance Msg OperationType where
  msg Query = msg ("query" :: TypeName)
  msg Mutation = msg ("mutation" :: TypeName)
  msg Subscription = msg ("subscription" :: TypeName)

type QUERY = 'Query

type MUTATION = 'Mutation

type SUBSCRIPTION = 'Subscription

-- | Document Reference with its Position
--
-- Position is used only for error messages. that means:
--
-- Ref "a" 1 === Ref "a" 3
data Ref name = Ref
  { refName :: name,
    refPosition :: Position
  }
  deriving (Show, Lift, Eq)

instance Ord name => Ord (Ref name) where
  compare (Ref x _) (Ref y _) = compare x y

isNotSystemTypeName :: TypeName -> Bool
isNotSystemTypeName =
  ( `notElem`
      [ "__Schema",
        "__Type",
        "__Directive",
        "__TypeKind",
        "__Field",
        "__DirectiveLocation",
        "__InputValue",
        "__EnumValue",
        "String",
        "Float",
        "Int",
        "Boolean",
        "ID"
      ]
  )
{-# INLINE isNotSystemTypeName #-}

sysFields :: [FieldName]
sysFields = ["__typename", "__schema", "__type"]
{-# INLINE sysFields #-}

hsTypeName :: TypeName -> TypeName
hsTypeName "String" = "Text"
hsTypeName "Boolean" = "Bool"
hsTypeName "Float" = "Double"
hsTypeName name = name
{-# INLINE hsTypeName #-}

toOperationType :: TypeName -> Maybe OperationType
toOperationType "Subscription" = Just Subscription
toOperationType "Mutation" = Just Mutation
toOperationType "Query" = Just Query
toOperationType _ = Nothing
{-# INLINE toOperationType #-}

-- handle reserved Names
isReserved :: FieldName -> Bool
isReserved "case" = True
isReserved "class" = True
isReserved "data" = True
isReserved "default" = True
isReserved "deriving" = True
isReserved "do" = True
isReserved "else" = True
isReserved "foreign" = True
isReserved "if" = True
isReserved "import" = True
isReserved "in" = True
isReserved "infix" = True
isReserved "infixl" = True
isReserved "infixr" = True
isReserved "instance" = True
isReserved "let" = True
isReserved "module" = True
isReserved "newtype" = True
isReserved "of" = True
isReserved "then" = True
isReserved "type" = True
isReserved "where" = True
isReserved "_" = True
isReserved _ = False
{-# INLINE isReserved #-}

convertToJSONName :: FieldName -> FieldName
convertToJSONName (FieldName hsName)
  | not (T.null hsName) && isReserved (FieldName name) && (T.last hsName == '\'') = FieldName name
  | otherwise = FieldName hsName
  where
    name = T.init hsName
{-# INLINE convertToJSONName #-}

convertToHaskellName :: FieldName -> FieldName
convertToHaskellName name
  | isReserved name = name <> "'"
  | otherwise = name
{-# INLINE convertToHaskellName #-}

unitTypeName :: TypeName
unitTypeName = "Unit"
{-# INLINE unitTypeName #-}

unitFieldName :: FieldName
unitFieldName = "_"
{-# INLINE unitFieldName #-}
