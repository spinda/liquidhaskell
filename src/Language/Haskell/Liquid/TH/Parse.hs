module Language.Haskell.Liquid.TH.Parse (
    parseDecs
  , parseType
  ) where

import Language.Haskell.TH.Syntax

import Text.Parsec (many)

import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.TH.Parse.Base
import Language.Haskell.Liquid.TH.Parse.Dec
import Language.Haskell.Liquid.TH.Parse.Type

--------------------------------------------------------------------------------
-- Top-Level Entry Points ------------------------------------------------------
--------------------------------------------------------------------------------

parseDecs :: Bool -> String -> Q [Dec]
parseDecs simplified = runParser simplified $ concat <$> many decP

parseType :: Bool -> String -> Q (Located AnnoType)
parseType simplified = runParser simplified typeP

