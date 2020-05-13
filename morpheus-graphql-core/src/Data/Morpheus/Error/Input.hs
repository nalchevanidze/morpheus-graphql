{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Input
  ( typeViolation,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
-- MORPHEUS

import Data.Morpheus.Types.Internal.AST
  ( Message,
    ResolvedValue,
    TypeRef (..),
    msg,
  )
import Data.Semigroup ((<>))

typeViolation :: TypeRef -> ResolvedValue -> Message
typeViolation expected found =
  "Expected type "
    <> msg expected
    <> " found "
    <> msg found
    <> "."

{-
  ARGUMENTS:
    type Experience {
        experience ( lang: LANGUAGE ) : String ,
        date: String
    }

  - required field !?
  - experience( lang: "bal" ) -> "Expected type LANGUAGE, found \"a\"."
  - experience( lang: Bla ) -> "Expected type LANGUAGE, found Bla."
  - experience( lang: 1 ) -> "Expected type LANGUAGE, found 1."
  - experience( a1 : 1 ) -> "Unknown argument \"a1\" on field \"experience\" of type \"Experience\".",
  - date(name: "name") -> "Unknown argument \"name\" on field \"date\" of type \"Experience\"."
-}
