{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Compile
  ( compileSyntax
  ) where

import qualified Data.Text                                as T (pack)
import           Language.Haskell.TH

--
--  Morpheus
import           Data.Morpheus.Error.Client.Client        (renderGQLErrors)
import           Data.Morpheus.Execution.Document.Convert (renderTHTypes)
import           Data.Morpheus.Parsing.Document.Parser    (parseTypes)

compileSyntax :: String -> Q Exp
compileSyntax documentTXT =
  case parseTypes (T.pack documentTXT) >>= renderTHTypes of
    Left errors -> fail (renderGQLErrors errors)
    Right root  -> [|root|]
