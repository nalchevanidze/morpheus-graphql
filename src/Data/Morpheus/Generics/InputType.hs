{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables , AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.InputType
    ( GQLInput(..)
    )
where

import           Data.Morpheus.Types.Types      ( JSType(..)
                                                , MetaInfo(..)
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Morpheus.Generics.GenericInputType
                                                ( GQLInputObject(..)
                                                , GToEnum(..)

                                                )
import           GHC.Generics
import           Data.Data
import           Data.Morpheus.Types.Introspection ( createType , GQLTypeLib, GQL__InputValue(..), createInputValue)
import           Data.Map as M

class GQLInput a where
    decode :: JSType -> a
    default decode :: ( Show a  , Generic a, Data a , GToEnum (Rep a) ) => JSType -> a
    decode (JSEnum text) = to $ gToEnum text
   -- TODO:: write input Object Recognition
   -- decode (JSObject hashMap) to $ gToInput hashMap

    typeInfo :: Proxy a -> Text -> GQL__InputValue
    default typeInfo :: (Show a, Typeable a) => Proxy a -> Text -> GQL__InputValue
    typeInfo _ name  = createInputValue name typeName
             where typeName = (pack . show . typeOf) (undefined::a)

    intro :: Proxy a -> GQLTypeLib -> GQLTypeLib
    default intro :: (Show a, Typeable a) => Proxy a -> GQLTypeLib -> GQLTypeLib
    intro _  typeLib = do
            let typeName = (pack . show . typeOf) (undefined::a)
            case M.lookup typeName typeLib of
                Just _ -> typeLib
                Nothing -> insert typeName (createType typeName []) typeLib

instance GQLInput Text where
    decode  (JSString x) = x
    typeInfo _ name = createInputValue name "String"
    intro _  typeLib = typeLib

instance GQLInput Bool where
    decode  (JSBool x) = x
    typeInfo _ name = createInputValue name "Boolean"
    intro _  typeLib = typeLib

instance GQLInput Int where
    decode  (JSInt x) = x
    typeInfo _ name = createInputValue name "Int"
    intro _  typeLib = typeLib

instance (GQLInput a , Show a, Typeable a ) => GQLInput (Maybe a) where
    decode JSNull = Nothing
    decode x = Just (decode x)
    typeInfo _ name =  (typeInfo (Proxy :: Proxy a) name) { defaultValue = "Nothing" }
    intro _  typeLib = typeLib


