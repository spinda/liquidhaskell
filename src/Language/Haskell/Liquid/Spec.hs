{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec (
    -- * Produce Module GhcSpec
    makeGhcSpec

    -- * Post-Process Full GhcSpec
  , postProcess
  ) where

import GHC hiding (Located)

import Annotations
import CoreSyn
import DynFlags
import GhcMonad
import HscTypes
import NameSet
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

import Language.Haskell.Liquid.CoreToLogic (strengthenResult)
import Language.Haskell.Liquid.CmdLine ()
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Extract
import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------
-- Produce Module GhcSpec ------------------------------------------------------
--------------------------------------------------------------------------------

makeGhcSpec :: Config -> NameSet -> TypecheckedModule -> [Var] -> [Annotation] -> [CoreBind] -> GhcSpec -> Ghc GhcSpec
makeGhcSpec cfg exports mod vs anns cbs scope = do
  anns'                                     <- (anns ++) <$> tcSigOfAnnotations mod
  wiredIns                                  <- loadWiredIns
  (exprParams, tcEmbeds, inlines, measures) <- runExtractM doExtract anns' cbs
  let inlines'                               = M.union inlines $ tinlines scope
  let measures'                              = M.union measures $ meas scope
  ((tySigs, ctors, tySyns), freeSyms)       <- runReifyM doReify wiredIns (rtEnv scope) exprParams inlines' measures'
  let tySigs'                                = strengthenHaskellMeasures (mkVarSet $ M.keys measures) tySigs
  let freeSyms'                              = map (second (joinVar vs)) freeSyms
  return $ mempty { tySigs   = map (second dummyLoc) tySigs'
                  , ctors    = map (second dummyLoc) ctors
                  , exports  = exports
                  , tcEmbeds = tcEmbeds
                  , freeSyms = freeSyms'
                  , rtEnv    = tySyns
                  , tinlines = inlines
                  , meas     = measures
                  , config   = cfg
                  }
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

strengthenHaskellMeasures :: VarSet -> [(Var, SpecType)] -> [(Var, SpecType)]
strengthenHaskellMeasures meas = map go
  where
    go (var, ty)
      | var `elemVarSet` meas = (var, strengthenResult var ty)
      | otherwise             = (var, ty)

--------------------------------------------------------------------------------
-- Post-Process Full GhcSpec ---------------------------------------------------
--------------------------------------------------------------------------------

postProcess :: GhcSpec -> GhcSpec
postProcess spec@SP{..} =
  spec { ctors = meetDataConSpec meas ctors
       }


meetDataConSpec :: M.HashMap Var SpecMeasure -> [(Id, Located SpecType)] -> [(Id, Located SpecType)]
meetDataConSpec meas ctors =
  M.toList $ foldl' meetDataConSpec' (M.fromList ctors) (M.elems meas)

meetDataConSpec' :: M.HashMap Id (Located SpecType) -> SpecMeasure -> M.HashMap Var (Located SpecType)
meetDataConSpec' ctors M{..} = foldl' meetDataConSpec'' ctors defs

meetDataConSpec'' :: M.HashMap Id (Located SpecType) -> (Id, SpecType) -> M.HashMap Var (Located SpecType)
meetDataConSpec'' ctors (id, mty) = case M.lookup id ctors of
  Nothing  -> ctors
  Just dty -> M.insert id (meet mty <$> dty) ctors

