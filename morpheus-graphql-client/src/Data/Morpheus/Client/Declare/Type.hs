{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Declare.Type
  ( typeDeclarations,
  )
where

--
-- MORPHEUS
import Data.Morpheus.Client.Internal.Types
  ( ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Internal.TH
  ( declareTypeRef,
    isEnum,
    mkFieldName,
    mkTypeName,
    nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldDefinition (..),
    FieldName,
    TypeKind (..),
    TypeName,
  )
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
import Language.Haskell.TH

typeDeclarations :: TypeKind -> [ClientTypeDefinition -> Q Dec]
typeDeclarations KindScalar = []
typeDeclarations _ = [pure . declareType]

declareType :: ClientTypeDefinition -> Dec
declareType
  ClientTypeDefinition
    { clientTypeName = thName@TypeNameTH {namespace, typename},
      clientCons
    } =
    DataD
      []
      (mkConName namespace typename)
      []
      Nothing
      (declareCons thName clientCons)
      (map derive [''Generic, ''Show])
    where
      derive className = DerivClause Nothing [ConT className]

declareCons :: TypeNameTH -> [ConsD ANY] -> [Con]
declareCons TypeNameTH {namespace, typename} clientCons
  | isEnum clientCons = map consE clientCons
  | otherwise = map consR clientCons
  where
    consE ConsD {cName} = NormalC (mkConName namespace (typename <> cName)) []
    consR ConsD {cName, cFields} =
      RecC
        (mkConName namespace cName)
        (map declareField cFields)

declareField :: FieldDefinition ANY -> (Name, Bang, Type)
declareField FieldDefinition {fieldName, fieldType} =
  ( mkFieldName fieldName,
    Bang NoSourceUnpackedness NoSourceStrictness,
    declareTypeRef False fieldType
  )

mkConName :: [FieldName] -> TypeName -> Name
mkConName namespace = mkTypeName . nameSpaceType namespace
