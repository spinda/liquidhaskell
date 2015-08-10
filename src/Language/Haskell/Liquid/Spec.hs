{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec (
    -- * Produce Module GhcSpec
    makeGhcSpec

    -- * Post-Process GhcSpec
  , postProcess
  ) where

import GHC hiding (Located)

import Annotations
import CoreSyn
import DynFlags
import GhcMonad
import HscTypes
import NameSet
import Panic
import TcAnnotations
import TcRnMonad
import TysWiredIn
import Var
import VarEnv
import VarSet

import Control.Arrow
import Control.Monad

import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)

import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Check
import Language.Haskell.Liquid.Spec.CoreToLogic (strengthenResult)
import Language.Haskell.Liquid.Spec.Extract
import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------
-- Produce Module GhcSpec ------------------------------------------------------
--------------------------------------------------------------------------------

makeGhcSpec :: NameSet
            -> TypecheckedModule
            -> [Var]
            -> [Annotation]
            -> [CoreBind]
            -> GhcSpec
            -> Ghc GhcSpec
makeGhcSpec exports mod vs anns cbs scope = do
  let TcGblEnv{..} = fst $ tm_internals_ mod

  anns'                                     <- (anns ++) <$> tcSigOfAnnotations mod
  wiredIns                                  <- loadWiredIns
  (exprParams, tcEmbeds, inlines, measures) <- runExtractM doExtract tcg_src scope anns' cbs

  throwsGhc $ checkDupLogic (map fst inlines) (map fst measures)
           ++ checkDupEmbeds tcEmbeds
           ++ checkLocalEmbeds tcg_mod tcg_sig_of tcEmbeds

  -- TODO: Get rid of all this (wasteful) when merging ExtractM and ReifyM
  let inlinesMap  = M.fromList $ map (first  val) inlines
  let measuresMap = M.fromList $ map (first  val) measures
  let embedsMap   = M.fromList $ map (second val) tcEmbeds

  let inlinesMap'                            = M.union inlinesMap $ tinlines scope
  let measuresMap'                           = M.union measuresMap $ meas scope

  ((tySigs, ctors, tySyns), freeSyms)       <- runReifyM doReify wiredIns (rtEnv scope) exprParams inlinesMap' measuresMap'

  let tySigs'                                = strengthenHaskellMeasures (mkVarSet $ M.keys measuresMap) tySigs
  let freeSyms'                              = M.map (joinVar vs) freeSyms

  let modSpec                                = mempty { tySigs   = M.map dummyLoc tySigs' -- TODO: Pull out location info
                                                      , ctors    = M.map dummyLoc ctors
                                                      , exports  = exports
                                                      , tcEmbeds = embedsMap
                                                      , freeSyms = freeSyms'
                                                      , rtEnv    = tySyns
                                                      , tinlines = inlinesMap
                                                      , meas     = measuresMap
                                                      }
  let fullSpec                               = (mappend modSpec scope) { exports = exports }

  throwsGhc $ checkGhcSpec cbs fullSpec modSpec

  return fullSpec
  where
    doExtract = (,,,)
      <$> extractExprParams
      <*> extractTcEmbeds
      <*> extractInlines
      <*> extractMeasures
    doReify = (,,)
      <$> extractTySigs mod
      <*> extractCtors  mod
      <*> extractTySyns mod

-- | GHC throws away annotations when typechecking a .hs-boot or .hsig file
-- (hoping to get this fixed for 7.12). We need them, so hook in here to
-- extract them in that case.
tcSigOfAnnotations :: TypecheckedModule -> Ghc [Annotation]
tcSigOfAnnotations mod
  | isHsBootOrSig source = do
    hscEnv <- getSession
    liftIO $ initTcForLookup hscEnv $ tcAnnotations annDecls
  | otherwise = return []
  where
    source =
      ms_hsc_src $ pm_mod_summary $ tm_parsed_module mod
    Just (HsGroup { hs_annds = annDecls }, _, _, _) =
      tm_renamed_source mod

-- | The 'Var's we lookup in GHC don't always have the same 'TyVar's as the
-- 'Var's we're given, so return the original 'Var' when possible.
--
-- See @tests/pos/ResolvePred.hs@ for an example
joinVar :: [Var] -> Var -> Var
joinVar vs v = fromMaybe v $ find ((== showPpr v) . showPpr) vs

strengthenHaskellMeasures :: VarSet -> M.HashMap Var SpecType -> M.HashMap Var SpecType
strengthenHaskellMeasures meas = M.mapWithKey go
  where
    go var ty
      | var `elemVarSet` meas = strengthenResult var ty
      | otherwise             = ty

--------------------------------------------------------------------------------
-- Post-Process GhcSpec --------------------------------------------------------
--------------------------------------------------------------------------------

postProcess :: GhcSpec -> GhcSpec
postProcess spec@SP{..} = spec
  { ctors = meetDataConSpec ctors (M.elems meas)
  }


meetDataConSpec :: M.HashMap Id (Located SpecType) -> [SpecMeasure] -> M.HashMap Id (Located SpecType)
meetDataConSpec ctors = foldl' meetDataConSpec' ctors

meetDataConSpec' :: M.HashMap Id (Located SpecType) -> SpecMeasure -> M.HashMap Var (Located SpecType)
meetDataConSpec' ctors M{..} = foldl' meetDataConSpec'' ctors defs

meetDataConSpec'' :: M.HashMap Id (Located SpecType) -> (Id, SpecType) -> M.HashMap Var (Located SpecType)
meetDataConSpec'' ctors (id, mty) = flip (M.insert id) ctors $ case M.lookup id ctors of
  Nothing  -> dummyLoc mty
  Just dty -> meet mty <$> dty

