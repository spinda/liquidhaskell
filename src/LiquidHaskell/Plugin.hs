module LiquidHaskell.Plugin (
    plugin
  ) where

import GHC

import GhcMonad
import GhcPlugins hiding (getOpts)
import HscTypes
import Panic

import Control.DeepSeq

import Data.IORef
import Data.Maybe

import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Console.CmdArgs.Default
import System.IO.Unsafe

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Interface
import Language.Fixpoint.Misc
import Language.Fixpoint.Types (FixResult(..), Result(..), sinfo)

import qualified Language.Fixpoint.Config as FC

import Language.Haskell.Liquid.Annotate (mkOutput)
import Language.Haskell.Liquid.CmdLine
import Language.Haskell.Liquid.Constraint.Generate
import Language.Haskell.Liquid.Constraint.ToFixpoint
import Language.Haskell.Liquid.Constraint.Types
import Language.Haskell.Liquid.Errors
import Language.Haskell.Liquid.GhcInterface
import Language.Haskell.Liquid.TransformRec
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.DiffCheck as DC

--------------------------------------------------------------------------------
-- Embed LiquidHaskell as a GHC Plugin -----------------------------------------
--------------------------------------------------------------------------------

-- | GHC Plugin Entry Point
plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install
                       }

-- | Install LiquidHaskell Compiler Pass
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = do
  reinitializeGlobals
  cfg0 <- liftIO $ getOpts opts
  return (lhCompilerPass cfg0 : todo)

-- | LiquidHaskell Pipeline in a Compiler Pass
lhCompilerPass :: Config -> CoreToDo
lhCompilerPass cfg0 =
  CoreDoPluginPass "LiquidHaskell" $ nonRecursive (runGhcInCoreM . liquidMain cfg0)

-- | Embed Ghc in CoreM
runGhcInCoreM :: Ghc a -> CoreM a
runGhcInCoreM act = do
  env <- getHscEnv
  ref <- liftIO $ newIORef env
  liftIO $ unGhc act (Session ref)

-- | Main LiquidHaskell Pipeline
liquidMain :: Config -> ModGuts -> Ghc ModGuts
liquidMain cfg0 guts = do
  summ          <- getModSummary $ moduleName mod
  let target     = msHsFilePath summ
  (info, guts') <- getGhcInfo cfg0 target mod summ guts
  result        <- liftIO $ o_result <$> liquidOne target info
  case result of
    Safe               -> return guts'
    Unsafe       _     -> pgmError "LiquidHaskell returned UNSAFE"
    Crash        _ err -> panic $  "LiquidHaskell crash: " ++ err
    UnknownError   err -> panic $  "Unknown error in LiquidHaskell: " ++ err
  where
    mod = mg_module guts

--------------------------------------------------------------------------------
-- Avoid Recursive Plugin Execution --------------------------------------------
--------------------------------------------------------------------------------

-- | Top-Level Mutable Running Flag
pluginRunningRef :: IORef Bool
pluginRunningRef = unsafePerformIO $ newIORef False

-- | Run Plugin Avoiding Recursion
nonRecursive :: (ModGuts -> CoreM ModGuts) -> ModGuts -> CoreM ModGuts
nonRecursive f guts = do
  running <- liftIO $ readIORef pluginRunningRef
  if running
     then return guts
     else do liftIO $ writeIORef pluginRunningRef True
             guts' <- f guts
             liftIO $ writeIORef pluginRunningRef False
             return guts'

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
