{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Plugin.Driver (
    processModule
  ) where

import GHC

import CoreSyn
import Digraph
import DriverPhases (Phase(..))
import DriverPipeline (compileFile)
import DynFlags
import Finder
import GhcMonad
import HscTypes
import Module
import MonadUtils
import Panic
import PrelNames
import TcPluginM
import TcRnMonad (tcIsHsBootOrSig)
import Var

import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.State

import Data.Either
import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Console.CmdArgs.Default

import System.Directory
import System.FilePath

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Files (tempDirectory)
import Language.Fixpoint.Interface
import Language.Fixpoint.Misc
import Language.Fixpoint.Types (sinfo, FixResult (..), Result (..))

import qualified Language.Fixpoint.Config as FC

import Language.Haskell.Liquid.Annotate (mkOutput)
import Language.Haskell.Liquid.CmdLine
import Language.Haskell.Liquid.Constraint.Generate
import Language.Haskell.Liquid.Constraint.ToFixpoint
import Language.Haskell.Liquid.Constraint.Types
import Language.Haskell.Liquid.Errors
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Iface
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Spec (postProcess)
import Language.Haskell.Liquid.TransformRec
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.DiffCheck as DC

import Language.Haskell.Liquid.Plugin.Ghc
import Language.Haskell.Liquid.Plugin.Misc

--------------------------------------------------------------------------------
-- Run a Module Through the LiquidHaskell Pipeline -----------------------------
--------------------------------------------------------------------------------

processModule :: Config -> IfaceCache -> ModSummary -> TcPluginM ()
processModule cfg cache summary@(ModSummary {..}) = do
  tcPluginIO $ putStrLn $ "== " ++ showPpr (moduleName ms_mod) ++ " =="

  (pkgImports, homeImports) <- getAllImports summary
  pkgIfaceData              <- mapM (getPkgIface cache cfg) pkgImports
  homeIfaceData             <- tcPluginIO $ mapM (getHomeIface cache) homeImports

  let externData = pkgIfaceData ++ homeIfaceData
  let extern     = mconcat $ map ifaceSpec externData

  tcPluginIO $ whenLoud $ putStrLn $
    "Dependencies: " ++ render (pprintLongList $ map ifaceModule externData)

  let hsFile   = fromJust $ ml_hs_file ms_location
  fingerprint <- tcPluginIO $ mkIfaceFingerprint ms_mod hsFile externData

  (lqhiFile, lqhiLoaded) <- tryLoadTargetIface cache ms_location fingerprint
  let lqhiDestFile        = if noWriteIface cfg then Nothing else Just lqhiFile

  if lqhiLoaded then return () else do
    ghcInfo <- tcPluginGhc $ getGhcInfo cfg extern hsFile summary

    let ghcSpec   = spec ghcInfo
    let fullSpec  = postProcess $ (mappend extern ghcSpec) { exports = exports ghcSpec }
    let fullInfo  = ghcInfo { spec = fullSpec }
    let ifaceData = ID ms_mod fingerprint fullSpec

    let save   = tcPluginIO $ putTargetIface cache lqhiDestFile ifaceData
    let verify = do
          out <- tcPluginIO $ liquidOne hsFile fullInfo
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
         , homeImps
         )

getDeclImports :: ModSummary -> TcPluginM ([Module], [Module])
getDeclImports summary = do
  homeModules <- tcPluginHomeModules
  partitionEithers <$> mapM (ofDecl homeModules) (ms_textual_imps summary)
  where
    ofDecl homeModules (unLoc -> decl) = do
      mod <- tcPluginFindModule (unLoc $ ideclName decl) (ideclPkgQual decl)
      return $ if mod `elemModuleSet` homeModules then Right mod else Left mod

primImports :: [Module]
primImports = [gHC_PRIM, gHC_TYPES]

--------------------------------------------------------------------------------
-- LiquidHaskell Verification Pipeline -----------------------------------------
--------------------------------------------------------------------------------

liquidOne :: FilePath -> GhcInfo -> IO (Output Doc)
liquidOne target info =
  do donePhase Loud "Extracted Core using GHC"
     let cfg   = config $ spec info
     whenLoud  $ do putStrLn "**** Config **************************************************"
                    print cfg
     whenLoud  $ do putStrLn $ showpp info
                    putStrLn "*************** Original CoreBinds ***************************"
                    putStrLn $ showpp (cbs info)
     let cbs' = transformScope (cbs info)
     whenLoud  $ do donePhase Loud "transformRecExpr"
                    putStrLn "*************** Transform Rec Expr CoreBinds *****************"
                    putStrLn $ showpp cbs'
                    putStrLn "*************** Slicing Out Unchanged CoreBinds *****************"
     dc <- prune cfg cbs' target info
     let cbs'' = maybe cbs' DC.newBinds dc
     let info' = maybe info (\z -> info {spec = DC.newSpec z}) dc
     let cgi   = {-# SCC "generateConstraints" #-} generateConstraints $! info' {cbs = cbs''}
     cgi `deepseq` donePhase Loud "generateConstraints"
     out      <- solveCs cfg target cgi info' dc
     donePhase Loud "solve"
     let out'  = mconcat [maybe mempty DC.oldOutput dc, out]
     DC.saveResult target out'
     exitWithResult cfg target out'

checkedNames ::  Maybe DC.DiffCheck -> Maybe [String]
checkedNames dc          = concatMap names . DC.newBinds <$> dc
   where
     names (NonRec v _ ) = [showpp $ shvar v]
     names (Rec xs)      = map (shvar . fst) xs
     shvar               = showpp . varName

prune :: Config -> [CoreBind] -> FilePath -> GhcInfo -> IO (Maybe DC.DiffCheck)
prune cfg cbinds target info
  | not (null vs) = return . Just $ DC.DC (DC.thin cbinds vs) mempty sp
  | diffcheck cfg = DC.slice target cbinds sp
  | otherwise     = return Nothing
  where
    vs            = tgtVars sp
    sp            = spec info

solveCs :: Config -> FilePath -> CGInfo -> GhcInfo -> Maybe DC.DiffCheck -> IO (Output Doc)
solveCs cfg target cgi info dc
  = do finfo    <- cgInfoFInfo info cgi
       Result r sol <- solve fx finfo
       let names = checkedNames dc
       let warns = logErrors cgi
       let annm  = annotMap cgi
       let res   = ferr sol r
       let out0  = mkOutput cfg res sol annm
       return    $ out0 { o_vars = names } { o_errors  = warns} { o_result = res }
    where
       fx        = def { FC.solver  = fromJust (smtsolver cfg)
                       , FC.real    = real   cfg
                       , FC.native  = native cfg
                       , FC.srcFile = target
                       -- , FC.stats   = True
                       }
       ferr s r  = fmap (tidyError s) $ result $ sinfo <$> r

