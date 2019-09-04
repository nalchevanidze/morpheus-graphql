{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Execution.Document.Declare
  ( declareTypes
  ) where

import           Data.Semigroup                           ((<>))
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Document.GQLType (deriveGQLType)
import           Data.Morpheus.Execution.Internal.Declare (declareResolverType, declareType)
import           Data.Morpheus.Types.Internal.DataD       (GQLTypeD)

declareTypes :: [GQLTypeD] -> Q [Dec]
declareTypes = fmap concat . traverse declareGQLType

declareGQLType :: GQLTypeD -> Q [Dec]
declareGQLType gqlType@(typeD, gqlKind, argTypes) = do
  let types = declareResolverType gqlKind [] typeD : map (declareType []) argTypes
  typeClasses <- deriveGQLType gqlType
  pure $ types <> typeClasses
