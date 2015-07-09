{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Driver (
    processModules
  ) where

import GHC
import GHC.Paths

import CoreSyn
import Digraph
import DriverPhases (Phase(..))
import DriverPipeline (compileFile)
import DynFlags
import Finder
import Fingerprint
import GhcMonad
import HscTypes
import Module
import MonadUtils
import Panic
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
import Language.Haskell.Liquid.GhcInterface
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Iface
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.TransformRec
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.DiffCheck as DC

--------------------------------------------------------------------------------

processModules :: Config -> [FilePath] -> IO ()
processModules cfg0 targets = runGhc (Just libdir) $ do
  targets'           <- mapM (`guessTarget` Nothing) targets
  mapM_ addTarget targets'

  cfg                <- liftIO $ withCabal cfg0
  updateDynFlags cfg
  compileCFiles cfg

  load LoadAllTargets

  modGraph           <- depanal [] True
  let summaries       = flattenSCCs $ topSortModuleGraph False modGraph Nothing

  evalStateT (mapM_ (processModule cfg) summaries) mempty

updateDynFlags :: Config -> Ghc ()
updateDynFlags cfg = do
  df <- getSessionDynFlags
  let df' = df { importPaths  = idirs cfg ++ importPaths df
               , libraryPaths = idirs cfg ++ libraryPaths df
               , includePaths = idirs cfg ++ includePaths df
               , profAuto     = ProfAutoCalls
               , ghcLink      = LinkInMemory
               --FIXME: this *should* be HscNothing, but that prevents us from
               -- looking up *unexported* names in another source module..
               , hscTarget    = HscInterpreted -- HscNothing
               , ghcMode      = CompManager
               -- prevent GHC from printing anything
               , log_action   = \_ _ _ _ _ -> return ()
               -- , verbosity = 3
               } `xopt_set` Opt_MagicHash
             --     `gopt_set` Opt_Hpc
                 `gopt_set` Opt_ImplicitImportQualified
                 `gopt_set` Opt_PIC
#if __GLASGOW_HASKELL__ >= 710
                 `gopt_set` Opt_Debug
#endif
  (df'',_,_) <- parseDynamicFlags df' (map noLoc $ ghcOptions cfg)
  void $ setSessionDynFlags $ df'' -- {profAuto = ProfAutoAll}

compileCFiles :: Config -> Ghc ()
compileCFiles cfg = do
  df  <- getSessionDynFlags
  setSessionDynFlags $ df { includePaths = nub $ idirs cfg ++ includePaths df
                          , importPaths  = nub $ idirs cfg ++ importPaths df
                          , libraryPaths = nub $ idirs cfg ++ libraryPaths df }
  hsc <- getSession
  os  <- mapM (\x -> liftIO $ compileFile hsc StopLn (x,Nothing)) (nub $ cFiles cfg)
  df  <- getSessionDynFlags
  void $ setSessionDynFlags $ df { ldInputs = map (FileOption "") os ++ ldInputs df }

--------------------------------------------------------------------------------

type ProcessM = StateT (M.HashMap Module (IfaceData GhcSpec)) Ghc

processModule :: Config -> ModSummary -> ProcessM ()
processModule cfg summary@(ModSummary { ms_mod = mod }) = do
  liftIO $ putStrLn $ "== " ++ showPpr mod ++ " =="

  (pkgImports, homeImports) <- lift $ getAllImports summary
  externData                <- assembleExternData pkgImports homeImports
  let extern                 = mconcat $ map ifaceSpec externData

  liftIO $ whenLoud $ putStrLn $
    "Dependencies: " ++ render (pprintLongList $ map ifaceModule externData)

  let hsFile   = fromJust $ ml_hs_file $ ms_location summary
  let lqhiFile = ifacePathWithHi $ ms_location summary

  fingerprint      <- liftIO $ getFileHash hsFile
  let dependencies  = map (ifaceModule &&& ifaceFingerprint) externData

  ghcIface    <- lift $ (fromJust . modInfoIface . fromJust) <$> getModuleInfo mod
  let ifaceMod = fromMaybe mod $ mi_sig_of ghcIface

  let ifExists True = do
        ifaceData <- readIfaceData lqhiFile
        if validateIfaceData ifaceMod fingerprint dependencies ifaceData
           then loadIfaceData ifaceData
           else ifExists False

      ifExists False = do
        ghcInfo  <- getGhcInfo cfg extern hsFile summary

        let ghcSpec   = spec ghcInfo
        let ghcSpec'  = mappend extern ghcSpec
        let ghcInfo'  = ghcInfo { spec = ghcSpec' }
        -- TODO: Include re-exported things in saved spec
        let ifaceData = ID ifaceMod fingerprint dependencies $ ghcSpec
                        { tcEmbeds = mappend (tcEmbeds ghcSpec') (tcEmbeds ghcSpec)
                        }

        out <- liftIO $ liquidOne hsFile ghcInfo'
        case o_result out of
          Safe | noWriteIface cfg -> return ifaceData
               | otherwise        -> ifaceData <$ liftIO (writeIfaceData lqhiFile ifaceData)
          Crash        _ err      -> panic      $ "LiquidHaskell crash: "         ++ err
          Unsafe       _          -> pgmError   $ "LiquidHaskell reported UNSAFE"
          UnknownError   err      -> panic      $ "LiquidHaskell unknown error: " ++ err

  ifaceData <- lift $ ifExists =<< liftIO (doesFileExist lqhiFile)
  modify $ M.insert mod ifaceData

assembleExternData :: [Module] -> [Module] -> ProcessM [IfaceData GhcSpec]
assembleExternData pkgImports homeImports = do
  (pkgUncached, pkgCached ) <- partitionEithers <$> mapM tryLoadCached pkgImports
  ([], homeCached)          <- partitionEithers <$> mapM tryLoadCached homeImports
  pkgLoaded                 <- lift $ mapM tryLoadFromFile pkgUncached
  return $ sortBy ifaceDataCmp (pkgLoaded ++ pkgCached ++ homeCached)
  where
    ifaceDataCmp x y = stableModuleCmp (ifaceModule x) (ifaceModule y)
    tryLoadCached mod = do
      cached <- gets $ M.lookup mod
      return $ case cached of
        Nothing        -> Left mod
        Just ifaceData -> Right ifaceData
    tryLoadFromFile mod = do
      maybePath <- findPkgIface mod
      case maybePath of
        Nothing       -> return $ emptyIfaceData mod
        Just lqhiFile -> loadIfaceData =<< readIfaceData lqhiFile

--------------------------------------------------------------------------------

getAllImports :: ModSummary -> Ghc ([Module], [Module])
getAllImports summary = do
  (declPkgImps, declHomeImps)   <- getDeclImports summary
  (usagePkgImps, usageHomeImps) <- getUsageImports $ ms_mod summary
  return ( nub $ declPkgImps ++ usagePkgImps
         , nub $ declHomeImps ++ usageHomeImps
         )

getDeclImports :: ModSummary -> Ghc ([Module], [Module])
getDeclImports summary = do
  partitionEithers <$> mapM ofDecl (ms_textual_imps summary)
  where
    ofDecl (unLoc -> decl) = do
      mod  <- findModule (unLoc $ ideclName decl) (ideclPkgQual decl)
      home <- isHomeModule mod
      return $ if home then Right mod else Left mod
    isHomeModule = isHomePackage . modulePackageKey
    isHomePackage pkg = do
      homePkg <- thisPackage <$> getSessionDynFlags
      liftIO $ putStrLn $ showPpr pkg ++ " vs " ++ showPpr homePkg
      return (homePkg == pkg)

getUsageImports :: Module -> Ghc ([Module], [Module])
getUsageImports mod = do
  homePkg <- thisPackage <$> getSessionDynFlags
  info    <- getModuleInfo mod
  return $ case info of
    Just (modInfoIface -> Just iface) ->
      partitionEithers $ mapMaybe (ofUsage homePkg) $ mi_usages iface
    _ -> ([], [])
  where
    ofUsage _ (UsagePackageModule { usg_mod = usedMod }) =
      Just $ Left usedMod
    ofUsage homePkg (UsageHomeModule { usg_mod_name = usedModName }) =
      Just $ Right $ mkModule homePkg usedModName
    ofUsage _ (UsageFile {}) =
      Nothing

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

