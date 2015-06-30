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

import Control.Arrow

import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)

import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Extract
import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------

makeGhcSpec :: Config -> NameSet -> TypecheckedModule -> [Var] -> [Annotation] -> [CoreBind] -> Ghc GhcSpec
makeGhcSpec cfg exports mod vs anns cbs = do
  liftIO $ whenLoud $ putStrLn "extraction started..."
  wiredIns                        <- loadWiredIns
  (exprParams, tcEmbeds, inlines) <- runExtractM doExtract anns cbs
  ((tySigs, tySyns), freeSyms)    <- runReifyM doReify wiredIns exprParams inlines
  let freeSyms' = map (second (joinVar vs)) freeSyms
  liftIO $ mapM_ printTySyn tySyns
  liftIO $ mapM_ printInline $ M.toList inlines
  return $ (emptySpec cfg) { tySigs   = map (second dummyLoc) tySigs
                           , exports  = exports
                           , tcEmbeds = tcEmbeds
                           , freeSyms = freeSyms'
                           }
  where
    doExtract = (,,)
      <$> extractExprParams
      <*> extractTcEmbeds
      <*> extractInlines
    doReify = (,)
      <$> extractTySigs mod
      <*> extractTySyns mod

    printTySyn (RTA name targs vargs body _ _) = whenLoud $
      do putStrLn $ "=== type synonym: " ++ showpp name ++ " ==="
         putStrLn $ "targs: " ++ showpp targs
         putStrLn $ "vargs: " ++ showpp vargs
         putStrLn $ "body: " ++ showpp body
    printInline (id, TI params def) = whenLoud $ do
      putStrLn $ "=== inline: " ++ showpp id
      putStrLn $ "params: " ++ showpp params
      putStrLn $ "def: " ++ either showpp showpp def

emptySpec :: Config -> GhcSpec
emptySpec cfg =
  SP [] [] [] [] [] [] [] [] [] mempty [] [] [] [] mempty mempty mempty cfg mempty [] mempty mempty

-- the Vars we lookup in GHC don't always have the same tyvars as the Vars
-- we're given, so return the original var when possible.
-- see tests/pos/ResolvePred.hs for an example
joinVar :: [Var] -> Var -> Var
joinVar vs v = fromMaybe v $ find ((== showPpr v) . showPpr) vs

