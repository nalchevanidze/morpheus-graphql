{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Document.RenderHaskell
  ( renderHaskellDocument
  ) where

import           Data.ByteString.Lazy.Char8           (ByteString)
import           Data.Maybe                           (catMaybes)
import           Data.Semigroup                       ((<>))
import           Data.Text                            (Text, intercalate, pack, toUpper)
import qualified Data.Text                            as T (concat, head, tail)
import qualified Data.Text.Lazy                       as LT (fromStrict)
import           Data.Text.Lazy.Encoding              (encodeUtf8)

-- MORPHEUS
import           Data.Morpheus.Document.Render.Therms (indent, renderAssignment, renderCon, renderData, renderList,
                                                       renderMaybe, renderTuple)
import           Data.Morpheus.Types.Internal.Data    (DataArgument, DataField (..), DataFullType (..), DataLeaf (..),
                                                       DataType (..), DataTypeLib, DataTypeWrapper (..), allDataTypes)

renderHaskellDocument :: DataTypeLib -> ByteString
renderHaskellDocument lib = encodeText $ renderLanguageExtensions <> renderExports <> renderImports <> types
  where
    encodeText = encodeUtf8 . LT.fromStrict
    types = intercalate "\n\n" $ map renderHaskellType visibleTypes
    visibleTypes = allDataTypes lib

renderExports :: Text
renderExports = "-- generated by 'Morpheus' CLI\n" <> "module Schema where\n\n"

unionType :: [Text] -> Text
unionType ls = "\n" <> indent <> intercalate ("\n" <> indent <> "| ") ls <> " deriving (Generic)"

renderLanguageExtensions :: Text
renderLanguageExtensions = T.concat (map renderExtension extensions) <> "\n"
  where
    renderExtension name = "{-# LANGUAGE " <> name <> " #-}\n"
    extensions = ["DeriveGeneric", "TypeFamilies"]

renderImports :: Text
renderImports = T.concat (map renderImport imports) <> "\n"
  where
    renderImport (src, list) = "import  " <> src <> "  (" <> intercalate ", " list <> ")\n"
    --------------------------------------------------------------------------------------
    imports =
      [ ("GHC.Generics", ["Generic"])
      , ("Data.Morpheus.Kind", ["SCALAR", "ENUM", "INPUT_OBJECT", "OBJECT", "UNION"])
      , ("Data.Morpheus.Types", ["ResM", "GQLType(..)"])
      ]

renderHaskellType :: (Text, DataFullType) -> Text
renderHaskellType (name, dataType) = typeIntro <> renderData name <> renderType dataType
  where
    renderType (Leaf (LeafScalar _)) = renderCon name <> "Int String" <> defineTypeClass "SCALAR"
    renderType (Leaf (LeafEnum DataType {typeData})) = unionType typeData <> defineTypeClass "ENUM"
    renderType (Union DataType {typeData}) = renderUnion name typeData <> defineTypeClass "UNION"
    renderType (InputObject DataType {typeData}) =
      renderCon name <> renderDataObject renderInputField typeData <> defineTypeClass "INPUT_OBJECT"
    renderType (InputUnion _) = "\n -- Error: Input Union Not Supported"
    renderType (OutputObject DataType {typeData}) =
      renderCon name <> renderDataObject renderField typeData <> defineTypeClass "OBJECT"
    ----------------------------------------------------------------------------------------------------------
    typeIntro = "\n\n---- GQL " <> name <> " ------------------------------- \n"
    ----------------------------------------------------------------------------------------------------------
    defineTypeClass kind =
      "\n\n" <> "instance GQLType " <> name <> " where\n" <> indent <> "type KIND " <> name <> " = " <> kind

renderUnion :: Text -> [DataField ()] -> Text
renderUnion typeName = unionType . map renderElem
  where
    renderElem DataField {fieldType} = renderCon (typeName <> "_" <> toUpper fieldType) <> fieldType

renderObject :: (a -> (Text, Maybe Text)) -> [a] -> Text
renderObject f list = intercalate "\n\n" $ renderMainType : catMaybes types
  where
    renderMainType = "\n  { " <> intercalate ("\n  ," <> indent) fields <> "\n  } deriving (Generic)"
    (fields, types) = unzip (map f list)

renderDataObject :: ((Text, DataField a) -> (Text, Maybe Text)) -> [(Text, DataField a)] -> Text
renderDataObject f list = renderObject f (ignoreHidden list)
  where
    ignoreHidden :: [(Text, DataField a)] -> [(Text, DataField a)]
    ignoreHidden = filter (not . fieldHidden . snd)

renderWrappedType :: [DataTypeWrapper] -> Text -> Text
renderWrappedType [] typeName                          = renderMaybe typeName
renderWrappedType [NonNullType] typeName               = typeName
renderWrappedType (NonNullType:(ListType:xs)) typeName = renderList $ renderWrappedType xs typeName
renderWrappedType (ListType:xs) typeName               = renderMaybe $ renderList $ renderWrappedType xs typeName
renderWrappedType (NonNullType:xs) typeName            = renderWrappedType xs typeName

renderInputField :: (Text, DataField ()) -> (Text, Maybe Text)
renderInputField (key, DataField {fieldTypeWrappers, fieldType}) =
  (key `renderAssignment` renderWrappedType fieldTypeWrappers fieldType, Nothing)

renderField :: (Text, DataField [(Text, DataArgument)]) -> (Text, Maybe Text)
renderField (key, DataField {fieldTypeWrappers, fieldType, fieldArgs}) =
  (key `renderAssignment` argTypeName <> " -> ResM " <> result fieldTypeWrappers, argTypes)
  where
    result wrappers@(NonNullType:_) = renderWrappedType wrappers fieldType
    result wrappers                 = renderTuple (renderWrappedType wrappers fieldType)
    (argTypeName, argTypes) = renderArguments fieldArgs
    renderArguments :: [(Text, DataArgument)] -> (Text, Maybe Text)
    renderArguments [] = ("()", Nothing)
    renderArguments list =
      ( fieldArgTypeName
      , Just (renderData fieldArgTypeName <> renderCon fieldArgTypeName <> renderDataObject renderInputField list))
      where
        fieldArgTypeName = "Arg" <> camelCase key
        camelCase :: Text -> Text
        camelCase ""   = ""
        camelCase text = toUpper (pack [T.head text]) <> T.tail text
