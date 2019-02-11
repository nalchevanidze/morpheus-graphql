{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.MorpheusGraphQL
    ( interpreter
    , GQLResponce
    , GQLRecord
    , GQLRoot
    , GQLArgs
    , (::->)(..)
    , GQLRequest(..)
    , eitherToResponce
    , Eval
    , EvalIO(..)
    , liftIO
    )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.GraphqlHS.Generics.GQLRecord
                                                ( GQLRecord )
import           Data.GraphqlHS.Generics.GQLRoot
                                                ( GQLRoot(decode) )
import           Data.GraphqlHS.Generics.GQLArgs
                                                ( GQLArgs )
import           Data.GraphqlHS.Parser.Parser   ( parseGQL )
import           Data.GraphqlHS.Types.Types     ( (::->)(Resolver)
                                                , GQLResponce
                                                , GQLRequest(..)
                                                , Eval(..)
                                                , EvalIO(..)
                                                )
import           Data.Proxy                     ( Proxy )
import           Control.Monad                  ( (>=>) )
import           Data.GraphqlHS.ErrorMessage    ( errorMessage )
import           Control.Monad.Trans.Except
import           Control.Monad.Trans            ( lift )


bla (Left  x) = Fail x
bla (Right x) = Val x

interpreter
    :: GQLRoot a => Proxy a -> IO (Eval a) -> GQLRequest -> IO GQLResponce
interpreter schema rootValue x = do
    rv <- rootValue
    case rv of
        Val rvx -> case (parseGQL . query) x of
            Val  x -> bla <$> runExceptT (decode rvx x)
            Fail x -> pure (Fail x)
        Fail x -> pure (Fail x)


liftIO :: IO a -> EvalIO a
liftIO = lift

eitherToResponce :: (a -> a) -> Either String a -> EvalIO a
eitherToResponce f (Left  x) = ExceptT $ pure $ Left $ errorMessage $ pack $ x
eitherToResponce f (Right x) = pure (f x)
