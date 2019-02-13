{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.GraphqlHS.PreProcess
    ( validateBySchema
    )
where

import           Prelude                 hiding ( lookup )
import           Data.List                      ( find )
import           Data.Map                       ( elems
                                                , mapWithKey
                                                , lookup
                                                , toList
                                                , Map
                                                , fromList
                                                , keys
                                                )
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text(..)
                                                , pack
                                                , unpack
                                                )
import           Data.GraphqlHS.Types.Types     ( Eval(..)
                                                , (::->)(..)
                                                , GQLType
                                                , QuerySelection(..)
                                                , SelectionSet
                                                , Head(..)
                                                , FragmentLib
                                                , Fragment(..)
                                                , MetaInfo(..)
                                                , Arg(..)
                                                , GQLPrimitive(..)
                                                , GQLQueryRoot(..)
                                                )
import           Data.GraphqlHS.ErrorMessage    ( semanticError
                                                , handleError
                                                , cannotQueryField
                                                , requiredArgument
                                                )

import           Data.Proxy
import           Data.GraphqlHS.Types.Introspection
                                                ( GQL__Type(fields, name)
                                                , GQL__Field
                                                , emptyLib
                                                , GQLTypeLib
                                                , GQL__InputValue
                                                )
import           Data.GraphqlHS.Schema.SchemaField
                                                ( getFieldTypeByKey
                                                , selectFieldBykey
                                                , fieldArgsByKey
                                                )
import           Data.GraphqlHS.Schema.InputValue
                                                ( inputValueName )

existsType :: Text -> GQLTypeLib -> Eval GQL__Type
existsType typeName typeLib = case (lookup typeName typeLib) of
    Nothing -> handleError $ pack $ "type does not exist" ++ (unpack typeName)
    Just x  -> pure x


validateSpread :: FragmentLib -> Text -> Eval [(Text, QuerySelection)]
validateSpread frags key = case lookup key frags of
    Nothing -> handleError $ pack $ "Fragment not found: " ++ (show key)
    Just (Fragment _ _ (SelectionSet gqlObj)) -> pure (toList gqlObj)


-- TODO: replace all var types with Variable values
replaceVariable :: GQLQueryRoot -> Arg -> Eval Arg
replaceVariable root (Var key) = case (lookup key (inputVariables root)) of
    Nothing    -> handleError $ pack $ "Variable not found: " ++ (show key)
    Just value -> pure $ ArgValue $ JSString value
replaceVariable _ x = pure $ x

validateArg
    :: GQLQueryRoot -> Map Text Arg -> GQL__InputValue -> Eval (Text, Arg)
validateArg root requestArgs inpValue =
    case (lookup (inputValueName inpValue) requestArgs) of
        Nothing -> Left $ requiredArgument $ MetaInfo
            { className = ""
            , cons      = ""
            , key       = (pack $ show $ inputValueName inpValue)
            }
        Just x -> replaceVariable root x >>= \x -> pure (key, x)
            where key = inputValueName inpValue



-- TODO: throw Error when gql request has more arguments al then inputType
validateHead :: GQLQueryRoot -> GQL__Type -> Text -> Head -> Eval Head
validateHead root currentType key (Arguments args) =
    case (fieldArgsByKey key currentType) of
        Nothing -> Left $ cannotQueryField key (name currentType)
        Just field ->
            mapM (validateArg root args) field >>= pure . Head . fromList

validateFieldBody
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Eval (Text, QuerySelection)
validateFieldBody typeLib root currentType (fieldName, field) =
    case (fields currentType) of
        Some gqlVal -> case (getFieldTypeByKey fieldName currentType) of
            Nothing -> Left $ cannotQueryField fieldName (name currentType)
            Just fieldType -> do
                value <- validateBySchema typeLib (name fieldType) root field
                pure (fieldName, value)
        _ ->
            handleError $ pack $ "has not fields" ++ (show $ fields currentType)

handleField
    :: GQLTypeLib
    -> GQLQueryRoot
    -> GQL__Type
    -> (Text, QuerySelection)
    -> Eval (Text, QuerySelection)
handleField typeLib root currentType (fieldName, field) = case field of
    SelectionSet head0 body0 -> do
        body <- validateFieldBody typeLib root currentType (fieldName, body0)
        pure $ (fieldName, SelectionSet head (snd body))
    _ -> validateFieldBody typeLib root currentType (fieldName, field)


propagateSpread
    :: GQLQueryRoot -> (Text, QuerySelection) -> Eval [(Text, QuerySelection)]
propagateSpread frags (key , (Spread _)) = validateSpread (fragments frags) key
propagateSpread frags (text, value     ) = pure [(text, value)]

validateBySchema
    :: GQLTypeLib
    -> Text
    -> GQLQueryRoot
    -> QuerySelection
    -> Eval QuerySelection
validateBySchema typeLib typeName root (SelectionSet head gqlObj) = do
    extended <- concat <$> (mapM (propagateSpread root) (toList gqlObj))
    existsType typeName typeLib >>= \_type -> do
        head' <- validateHead root _type fieldName head
        body' <- fromList <$> (mapM (handleField typeLib root _type) extended)
        pure (SelectionSet head' body')

validateBySchema _ _ _ (Field args value) = pure (Field args value)

validateBySchema _ _ _ QNull              = pure QNull
