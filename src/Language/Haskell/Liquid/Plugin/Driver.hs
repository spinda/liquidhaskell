{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Plugin.Driver (
    processModule
  ) where

import GHC

import CoreSyn
import Digraph
import DriverPhases
import DriverPipeline
import DynFlags
import Finder
import GhcMonad
import HscTypes
import Module
import MonadUtils
import Panic
import PipelineMonad
import PrelNames
import SysTools
import TcPluginM
import TcRnMonad (tcIsHsBootOrSig)
import Var

import Control.Arrow
import Control.Monad
import Control.Monad.State

import Data.Either
import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)

import System.Directory
import System.FilePath

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Iface
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Pipeline
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Spec
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Plugin.Misc

--------------------------------------------------------------------------------
-- Run a Module Through the LiquidHaskell Pipeline -----------------------------
--------------------------------------------------------------------------------

processModule :: Config -> IfaceCache -> ModSummary -> TcPluginM ()
processModule cfg cache summary@(ModSummary {..}) = do
  -- TODO: Redo LH's output so it fits nicely with GHC's messages
  tcPluginIO $ putStrLn $ "== " ++ showPpr (moduleName ms_mod) ++ " =="

  (pkgImports, homeImports) <- getAllImports summary
  pkgIfaceData              <- mapM (getPkgIface cache cfg) pkgImports
  homeIfaceData             <- mapM (getHomeIface cache) homeImports

  let externData = pkgIfaceData ++ homeIfaceData
  let extern     = mconcat $ map ifaceSpec externData

  tcPluginIO $ whenLoud $ putStrLn $
    "Dependencies: " ++ render (pprintLongList $ map ifaceModule externData)

  let hsFile   = fromJust $ ml_hs_file ms_location
  fingerprint <- tcPluginIO $ mkIfaceFingerprint ms_mod hsFile externData

  (lqhiFile, lqhiLoaded) <- tryLoadTargetIface cache ms_location fingerprint
  let lqhiDestFile        = if noWriteIface cfg then Nothing else Just lqhiFile

  if lqhiLoaded then return () else do
    ghcInfo       <- tcPluginGhc $ getGhcInfo cfg extern hsFile summary
    let ghcSpec    = spec ghcInfo
    let ifaceData  = ID ms_mod fingerprint ghcSpec
    let ghcInfo'   = ghcInfo { spec = postProcess ghcSpec }

    let save   = do
          tcPluginIO $ putTargetIface cache lqhiDestFile ifaceData
          fixHsigDynamicToo ms_mod ms_location
    let verify = do
          out <- tcPluginIO $ liquidOne hsFile ghcInfo'
          case o_result out of
            Safe               -> save
            Unsafe       _     -> pgmError "LiquidHaskell reported UNSAFE"
            Crash        _ err -> panic  $ "LiquidHaskell crash: "         ++ err
            UnknownError   err -> panic  $ "LiquidHaskell unknwon error: " ++ err

    isSig <- tcPluginIsHsBootOrSig

    if noVerify cfg || isSig then save else verify

--------------------------------------------------------------------------------
-- Extract Full Module Dependencies --------------------------------------------
--------------------------------------------------------------------------------

getAllImports :: ModSummary -> TcPluginM ([Module], [Module])
getAllImports summary = do
  (pkgImps, homeImps) <- getDeclImports summary
  return ( nub $ pkgImps ++ primImports
         , nub homeImps
         )

getDeclImports :: ModSummary -> TcPluginM ([Module], [Module])
getDeclImports summary = do
  homeModules <- tcPluginHomeModules
  partitionEithers <$> mapM (ofDecl homeModules)
                            (ms_textual_imps summary ++ ms_srcimps summary)
  where
    ofDecl homeModules (unLoc -> decl) = do
      mod <- tcPluginFindModule (unLoc $ ideclName decl) (ideclPkgQual decl)
      return $ if mod `elemModuleSet` homeModules then Right mod else Left mod

primImports :: [Module]
primImports = [gHC_PRIM, gHC_TYPES]

--------------------------------------------------------------------------------
-- Work Around .hsig -dynamic-too Bug in GHC 7.10 ------------------------------
--------------------------------------------------------------------------------

-- | In -dynamic-too mode, GHC 7.10 doesn't generate .dyn_o files for .hsig
-- sources, resulting in a linker error. Here we work around this by generating
-- them ourselves.
--
-- A patch is in, and this will be fixed in GHC 7.10.3 (if it exists) or GHC
-- 7.12.

fixHsigDynamicToo :: Module -> ModLocation -> TcPluginM ()
fixHsigDynamicToo mod loc = do
  hscEnv <- getTopEnv
  hscSrc <- tcPluginHscSource
  let dflags      = hsc_dflags hscEnv
  let Just hsFile = ml_hs_file loc
  when (hscSrc == HsigFile) $
    tcPluginIO $ whenGeneratingDynamicToo dflags $ do
      let env   = PipeEnv True StopLn hsFile (dropExtension hsFile) "hsig" Persistent
      let state = PipeState hscEnv (Just loc) Nothing
      (\act -> evalP act env state) $ do
        let dflags' = dynamicTooMkDynamicDynFlags dflags 
        setDynFlags dflags'
        void $ runPhase (HscOut hscSrc (moduleName mod) HscUpdateSig) hsFile dflags'

