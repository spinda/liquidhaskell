{-# LANGUAGE CPP #-}

module Language.Haskell.Liquid.Driver (
    processModules
  ) where

import GHC
import GHC.Paths

import CoreSyn
import DriverPhases (Phase(..))
import DriverPipeline (compileFile)
import DynFlags
import GhcMonad
import Panic
import Var

import Control.DeepSeq
import Control.Monad

import Data.List
import Data.Maybe

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
import Language.Haskell.Liquid.Iface
import Language.Haskell.Liquid.RefType
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

  summaries          <- depanal [] True
  foldM_ (processModule cfg) mempty summaries

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

processModule :: Config -> GhcSpec -> ModSummary -> Ghc GhcSpec
processModule cfg scope summary = do
  lqhiExists <- liftIO $ doesFileExist lqhiFile
  if lqhiExists
     then loadFromIface scope (ms_mod summary) lqhiFile
     else loadFromHsSource cfg scope hsFile lqhiFile summary
  where
    hsFile    = fromJust $ ml_hs_file $ ms_location summary
    liquidDir = tempDirectory hsFile
    lqhiFile  = liquidDir </> replaceExtension (takeFileName hsFile) "lqhi"

loadFromIface :: GhcSpec -> Module -> FilePath -> Ghc GhcSpec
loadFromIface scope mod lqhiFile = do
  load $ LoadUpTo (moduleName mod)
  mappend scope <$> readIfaceSpec lqhiFile mod

loadFromHsSource :: Config -> GhcSpec -> FilePath -> FilePath -> ModSummary -> Ghc GhcSpec
loadFromHsSource cfg scope hsFile lqhiFile summary = do
  ghcInfo <- getGhcInfo cfg scope hsFile summary
  let ghcSpec  = spec ghcInfo
  let ghcSpec' = mappend scope ghcSpec
  let ghcInfo' = ghcInfo { spec = ghcSpec' }
  out <- liftIO $ liquidOne hsFile ghcInfo'
  case o_result out of
    Safe                -> ghcSpec' <$ liftIO (writeIfaceSpec lqhiFile ghcSpec)
    Crash        _ err  -> panic     $ "LiquidHaskell crash: "         ++ err
    Unsafe       _      -> pgmError  $ "LiquidHaskell reported UNSAFE"
    UnknownError   err  -> panic     $ "LiquidHaskell unknown error: " ++ err

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

