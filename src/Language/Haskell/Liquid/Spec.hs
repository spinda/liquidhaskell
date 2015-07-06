{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec (
    GhcSpec(..)
  , makeGhcSpec
  ) where

import GHC

import Annotations
import CoreSyn
import GhcMonad
import HscTypes
import NameSet
import TysWiredIn
import Var
import VarEnv

import Control.Arrow

import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)

import Language.Fixpoint.Types

import Language.Haskell.Liquid.CmdLine ()
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Extract
import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------

makeGhcSpec :: Config -> NameSet -> TypecheckedModule -> [Var] -> [Annotation] -> [CoreBind] -> GhcSpec -> Ghc GhcSpec
makeGhcSpec cfg exports mod vs anns cbs scope = do
  liftIO $ whenLoud $ putStrLn "extraction started..."
  wiredIns                        <- loadWiredIns
  (exprParams, tcEmbeds, inlines) <- runExtractM doExtract anns cbs
  let inlines'                     = extendVarEnvList inlines $ tinlines scope
  ((tySigs, tySyns), freeSyms)    <- runReifyM doReify wiredIns (rtEnv scope) exprParams inlines'
  let freeSyms'                    = map (second (joinVar vs)) freeSyms
  return $ mempty { tySigs   = map (second dummyLoc) tySigs
                  , exports  = exports
                  , tcEmbeds = tcEmbeds
                  , freeSyms = freeSyms'
                  , rtEnv    = tySyns
                  , config   = cfg
                  }
  where
    doExtract = (,,)
      <$> extractExprParams
      <*> extractTcEmbeds
      <*> extractInlines
    doReify = (,)
      <$> extractTySigs mod
      <*> extractTySyns mod

-- the Vars we lookup in GHC don't always have the same tyvars as the Vars
-- we're given, so return the original var when possible.
-- see tests/pos/ResolvePred.hs for an example
joinVar :: [Var] -> Var -> Var
joinVar vs v = fromMaybe v $ find ((== showPpr v) . showPpr) vs

