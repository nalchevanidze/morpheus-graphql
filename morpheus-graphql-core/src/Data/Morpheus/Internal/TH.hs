{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.TH
  ( _',
    apply,
    applyCons,
    applyVars,
    declareTypeRef,
    funDSimple,
    nameSpaceField,
    nameSpaceType,
    toCon,
    toVar,
    ToName (..),
    toString,
    typeInstanceDec,
    v',
    vars,
  )
where

import Data.Morpheus.Internal.Utils
  ( capitalize,
    nameSpaceField,
    nameSpaceType,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName (..),
    TypeName (..),
    TypeRef (..),
    TypeWrapper (..),
    convertToHaskellName,
    readName,
  )
import Data.Text (unpack)
import Language.Haskell.TH
import Relude hiding
  ( ToString (..),
    Type,
  )

_' :: PatQ
_' = toVar (mkName "_")

v' :: ToVar Name a => a
v' = toVar (mkName "v")

declareTypeRef :: (TypeName -> Type) -> TypeRef -> Type
declareTypeRef f TypeRef {typeConName, typeWrappers} = wrappedT typeWrappers
  where
    wrappedT :: TypeWrapper -> Type
    wrappedT (TypeList xs nonNull) = withNonNull nonNull (AppT (ConT ''[]) $ wrappedT xs)
    wrappedT (BaseType nonNull) = withNonNull nonNull (f typeConName)
    {-# INLINE wrappedT #-}
{-# INLINE declareTypeRef #-}

withNonNull :: Bool -> Type -> Type
withNonNull True = id
withNonNull False = AppT (ConT ''Maybe)
{-# INLINE withNonNull #-}

cons :: ToCon a b => [a] -> [b]
cons = map toCon

vars :: ToVar a b => [a] -> [b]
vars = map toVar

class ToName a where
  toName :: a -> Name

instance ToName String where
  toName = mkName

instance ToName Name where
  toName = id

instance ToName TypeName where
  toName = mkName . unpack . capitalize . readTypeName

instance ToName FieldName where
  toName = mkName . unpack . readName . convertToHaskellName

class ToString a b where
  toString :: a -> b

instance ToString a b => ToString a (Q b) where
  toString = pure . toString

instance ToString TypeName Lit where
  toString = stringL . unpack . readTypeName

instance ToString TypeName Pat where
  toString = LitP . toString

instance ToString FieldName Lit where
  toString (FieldName x) = stringL (unpack x)

instance ToString TypeName Exp where
  toString = LitE . toString

instance ToString FieldName Exp where
  toString = LitE . toString

class ToCon a b where
  toCon :: a -> b

instance ToCon a b => ToCon a (Q b) where
  toCon = pure . toCon

instance (ToName a) => ToCon a Type where
  toCon = ConT . toName

instance (ToName a) => ToCon a Exp where
  toCon = ConE . toName

class ToVar a b where
  toVar :: a -> b

instance ToVar a b => ToVar a (Q b) where
  toVar = pure . toVar

instance (ToName a) => ToVar a Type where
  toVar = VarT . toName

instance (ToName a) => ToVar a Exp where
  toVar = VarE . toName

instance (ToName a) => ToVar a Pat where
  toVar = VarP . toName

class Apply a where
  apply :: ToCon i a => i -> [a] -> a

instance Apply TypeQ where
  apply = foldl' appT . toCon

instance Apply Type where
  apply = foldl' AppT . toCon

instance Apply Exp where
  apply = foldl' AppE . toCon

instance Apply ExpQ where
  apply = foldl' appE . toCon

applyVars ::
  ( ToName con,
    ToName var,
    Apply res,
    ToCon con res,
    ToVar var res
  ) =>
  con ->
  [var] ->
  res
applyVars name li = apply name (vars li)

applyCons :: (ToName con, ToName cons) => con -> [cons] -> Q Type
applyCons name li = apply name (cons li)

funDSimple :: Name -> [PatQ] -> ExpQ -> DecQ
funDSimple name args body = funD name [clause args (normalB body) []]

#if MIN_VERSION_template_haskell(2,15,0)
-- fix breaking changes
typeInstanceDec :: Name -> Type -> Type -> Dec
typeInstanceDec typeFamily arg res = TySynInstD (TySynEqn Nothing (AppT (ConT typeFamily) arg) res)
#else
--
typeInstanceDec :: Name -> Type -> Type -> Dec
typeInstanceDec typeFamily arg res = TySynInstD typeFamily (TySynEqn [arg] res)
#endif
