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
import HsDecls
import NameSet
import Panic
import RnSource
import TcAnnotations
import TcRnMonad
import TcRnTypes
import TysWiredIn
import Var
import VarEnv
import VarSet

import Control.Arrow
import Control.Monad

import Data.IORef
import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

import System.Console.CmdArgs.Verbosity (whenLoud)

import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Check
import Language.Haskell.Liquid.Spec.CoreToLogic (strengthenResult)
import Language.Haskell.Liquid.Spec.Extract
import Language.Haskell.Liquid.Spec.Expand
import Language.Haskell.Liquid.Spec.Resolve

--------------------------------------------------------------------------------
-- Produce Module GhcSpec ------------------------------------------------------
--------------------------------------------------------------------------------

makeGhcSpec :: NameSet
            -> TypecheckedModule
            -> [Var]
            -> [Var]
            -> [Annotation]
            -> [CoreBind]
            -> GhcSpec
            -> Ghc GhcSpec
makeGhcSpec exports mod vs letVs anns cbs scope = do
  let TcGblEnv{..} = fst $ tm_internals_ mod

  anns' <- (anns ++) <$> tcSigOfAnnotations mod

  spec0 <- extractGhcSpec tcg_mod tcg_sig_of letVs anns' cbs scope
  spec1 <- resolveGhcSpec spec0
  spec2 <- expandGhcSpec scope spec1

  let measSet = S.fromList $ map val $ M.keys $ meas spec2

  let modSpec  = spec2 { tySigs   = strengthenHaskellMeasures measSet $ tySigs   spec2
                       , asmSigs  = strengthenHaskellMeasures measSet $ asmSigs  spec2
                       , freeSyms = M.map (joinVar vs)                $ freeSyms spec2
                       }

  let fullSpec = (mappend modSpec scope) { exports = exports }

  throwsGhc $ checkGhcSpec cbs fullSpec modSpec

  return fullSpec


-- | GHC throws away annotations when typechecking a .hs-boot or .hsig file
-- (hoping to get this fixed for 7.12). We need them, so hook in here to
-- extract them in that case.
tcSigOfAnnotations :: TypecheckedModule -> Ghc [Annotation]
tcSigOfAnnotations mod
  | isHsBootOrSig source = do
    hscEnv <- getSession
    liftIO $ do
      extraDecls <- readIORef $ tcg_th_topdecls gblEnv
      initTcForLookup hscEnv $ do
        setGblEnv gblEnv $ do
          (extraGroupRdr, _) <- findSplice $ filter isAnnDecl extraDecls
          (_, extraGroupRn)  <- rnSrcDecls [] extraGroupRdr
          tcAnnotations $ groupDecls ++ hs_annds extraGroupRn
  | otherwise = return []
  where
    source =
      ms_hsc_src $ pm_mod_summary $ tm_parsed_module mod
    gblEnv =
      fst $ tm_internals_ mod
    Just (HsGroup { hs_annds = groupDecls }, _, _, _) =
      tm_renamed_source mod
    isAnnDecl decl = case unLoc decl of
      AnnD{} -> True
      _      -> False


-- | The 'Var's we lookup in GHC don't always have the same 'TyVar's as the
-- 'Var's we're given, so return the original 'Var' when possible.
--
-- See @tests/pos/ResolvePred.hs@ for an example
joinVar :: [Var] -> Var -> Var
joinVar vs v = fromMaybe v $ find ((== showPpr v) . showPpr) vs

strengthenHaskellMeasures :: S.HashSet Symbol -> M.HashMap Var (Located SpecType) -> M.HashMap Var (Located SpecType)
strengthenHaskellMeasures meas = M.mapWithKey go
  where
    go var ty
      | varSymbol var `S.member` meas = strengthenResult var <$> ty
      | otherwise                     = ty

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

