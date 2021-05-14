{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Value
  ( Value (..),
    ScalarValue (..),
    Object,
    replaceValue,
    decodeScientific,
    RawValue,
    ValidValue,
    RawObject,
    ValidObject,
    Variable (..),
    ResolvedValue,
    ResolvedObject,
    VariableContent (..),
    ObjectEntry (..),
    VariableDefinitions,
  )
where

-- MORPHEUS
import qualified Data.Aeson as A
  ( (.=),
    FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    pairs,
  )
import Data.Foldable (foldr')
import qualified Data.HashMap.Lazy as M
import Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
import Data.Morpheus.Ext.OrdMap
  ( OrdMap,
    unsafeFromList,
  )
import Data.Morpheus.Internal.Utils
  ( KeyOf (..),
    elems,
    mapTuple,
  )
import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
    fromText,
    render,
    renderInputSeq,
    space,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    FieldName (..),
    Msg (..),
    Position,
    Ref (..),
    TypeName (..),
    ValidationError (..),
    msgValidation,
  )
import Data.Morpheus.Types.Internal.AST.Stage
  ( CONST,
    CONST_OR_VALID,
    RAW,
    Stage,
    VALID,
  )
import Data.Morpheus.Types.Internal.AST.Type (TypeRef (..))
import Data.Scientific
  ( Scientific,
    floatingOrInteger,
  )
import qualified Data.Vector as V
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Relude

-- | Primitive Values for GQLScalar: 'Int', 'Float', 'String', 'Boolean'.
-- for performance reason type 'Text' represents GraphQl 'String' value
data ScalarValue
  = Int Int
  | Float Double
  | String Text
  | Boolean Bool
  | Value A.Value
  deriving (Show, Eq, Generic, Lift)

instance RenderGQL ScalarValue where
  renderGQL (Int x) = renderGQL x
  renderGQL (Float x) = renderGQL x
  renderGQL (String x) = renderGQL x
  renderGQL (Boolean x) = renderGQL x
  renderGQL (Value x) = renderGQL x

instance A.ToJSON ScalarValue where
  toJSON (Float x) = A.toJSON x
  toJSON (Int x) = A.toJSON x
  toJSON (Boolean x) = A.toJSON x
  toJSON (String x) = A.toJSON x
  toJSON (Value x) = A.toJSON x

instance A.FromJSON ScalarValue where
  parseJSON (A.Bool v) = pure $ Boolean v
  parseJSON (A.Number v) = pure $ decodeScientific v
  parseJSON (A.String v) = pure $ String v
  parseJSON notScalar = fail $ "Expected Scalar got :" <> show notScalar

data VariableContent (stage :: Stage) where
  DefaultValue :: Maybe ResolvedValue -> VariableContent CONST
  ValidVariableValue :: {validVarContent :: ValidValue} -> VariableContent VALID

instance Lift (VariableContent a) where
  lift (DefaultValue x) = [|DefaultValue x|]
  lift (ValidVariableValue x) = [|ValidVariableValue x|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (DefaultValue x) = [||DefaultValue x||]
  liftTyped (ValidVariableValue x) = [||ValidVariableValue x||]
#endif
deriving instance Show (VariableContent a)

deriving instance Eq (VariableContent a)

data Variable (stage :: Stage) = Variable
  { variablePosition :: Position,
    variableName :: FieldName,
    variableType :: TypeRef,
    variableValue :: VariableContent (CONST_OR_VALID stage)
  }
  deriving (Show, Eq, Lift)

instance KeyOf FieldName (Variable s) where
  keyOf = variableName

instance NameCollision (Variable s) where
  nameCollision Variable {variableName, variablePosition} =
    ValidationError
      { validationMessage = "There can Be only One Variable Named " <> msg variableName,
        validationLocations = [variablePosition]
      }

type VariableDefinitions s = OrdMap FieldName (Variable s)

data Value (stage :: Stage) where
  ResolvedVariable :: Ref FieldName -> Variable VALID -> Value CONST
  VariableValue :: Ref FieldName -> Value RAW
  Object :: Object stage -> Value stage
  List :: [Value stage] -> Value stage
  Enum :: TypeName -> Value stage
  Scalar :: ScalarValue -> Value stage
  Null :: Value stage

deriving instance Show (Value a)

deriving instance Eq (Value s)

data ObjectEntry (s :: Stage) = ObjectEntry
  { entryName :: FieldName,
    entryValue :: Value s
  }
  deriving (Eq, Show)

instance RenderGQL (ObjectEntry a) where
  renderGQL (ObjectEntry (FieldName name) value) = fromText name <> ": " <> renderGQL value

instance NameCollision (ObjectEntry s) where
  nameCollision ObjectEntry {entryName} =
    "There can Be only One field Named " <> msgValidation entryName :: ValidationError

instance KeyOf FieldName (ObjectEntry s) where
  keyOf = entryName

type Object a = OrdMap FieldName (ObjectEntry a)

type ValidObject = Object VALID

type RawObject = Object RAW

type ResolvedObject = Object CONST

type RawValue = Value RAW

type ValidValue = Value VALID

type ResolvedValue = Value CONST

deriving instance Lift (Value a)

deriving instance Lift (ObjectEntry a)

instance RenderGQL (Value a) where
  renderGQL (ResolvedVariable Ref {refName} _) = "$" <> renderGQL refName
  renderGQL (VariableValue Ref {refName}) = "$" <> renderGQL refName <> " "
  renderGQL Null = "null"
  renderGQL (Enum x) = renderGQL x
  renderGQL (Scalar x) = renderGQL x
  renderGQL (Object xs) = "{" <> entries <> "}"
    where
      entries
        | null xs = ""
        | otherwise = space <> renderInputSeq (elems xs) <> space
  renderGQL (List list) = "[" <> renderInputSeq list <> "]"

instance Msg (Value a) where
  msg = msg . render

instance A.ToJSON (Value a) where
  toJSON (ResolvedVariable _ Variable {variableValue = ValidVariableValue x}) =
    A.toJSON x
  toJSON (VariableValue Ref {refName}) =
    A.String $ "($ref:" <> readName refName <> ")"
  toJSON Null = A.Null
  toJSON (Enum (TypeName x)) = A.String x
  toJSON (Scalar x) = A.toJSON x
  toJSON (List x) = A.toJSON x
  toJSON (Object fields) = A.object $ fmap toEntry (elems fields)
    where
      toEntry (ObjectEntry (FieldName name) value) = name A..= A.toJSON value

  -------------------------------------------
  toEncoding (ResolvedVariable _ Variable {variableValue = ValidVariableValue x}) =
    A.toEncoding x
  toEncoding (VariableValue Ref {refName}) =
    A.toEncoding $ "($ref:" <> refName <> ")"
  toEncoding Null = A.toEncoding A.Null
  toEncoding (Enum x) = A.toEncoding x
  toEncoding (Scalar x) = A.toEncoding x
  toEncoding (List x) = A.toEncoding x
  toEncoding (Object ordMap) = A.pairs $ renderSeries encodeField (elems ordMap)
    where
      encodeField (ObjectEntry (FieldName key) value) = key A..= value

-- fixes GHC 8.2.2, which can't deduce (Semigroup p) from context (Monoid p)
renderSeries :: (Semigroup p, Monoid p) => (e -> p) -> [e] -> p
renderSeries _ [] = mempty
renderSeries f (x : xs) = foldr' (\e es -> es <> f e) (f x) xs

decodeScientific :: Scientific -> ScalarValue
decodeScientific v = case floatingOrInteger v of
  Left float -> Float float
  Right int -> Int int

replaceValue :: A.Value -> Value a
replaceValue (A.Bool v) = mkBoolean v
replaceValue (A.Number v) = Scalar $ decodeScientific v
replaceValue (A.String v) = mkString v
replaceValue (A.Object v) =
  mkObject $
    fmap
      (mapTuple FieldName replaceValue)
      (M.toList v)
replaceValue (A.Array li) = List (fmap replaceValue (V.toList li))
replaceValue A.Null = Null

instance A.FromJSON (Value a) where
  parseJSON = pure . replaceValue

-- DEFAULT VALUES

mkBoolean :: Bool -> Value s
mkBoolean = Scalar . Boolean

mkString :: Text -> Value s
mkString = Scalar . String

mkObject :: [(FieldName, Value s)] -> Value s
mkObject = Object . unsafeFromList . fmap toEntry
  where
    toEntry (key, value) = (key, ObjectEntry key value)
