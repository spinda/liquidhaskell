{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS_GHC -fno-cse #-}

{-@ LIQUID "--diff"     @-}

-- | This module contains all the code needed to output the result which
--   is either: `SAFE` or `WARNING` with some reasonable error message when
--   something goes wrong. All forms of errors/exceptions should go through
--   here. The idea should be to report the error, the source position that
--   causes it, generate a suitable .json file and then exit.


module Language.Haskell.Liquid.CmdLine (
   -- * Get Command Line Configuration
     getOpts, mkOpts

   -- * Update Configuration With Pragma
   , withPragmas

   -- * Exit Function
   , exitWithResult

   -- * Diff check mode
   , diffcheck
) where

import Control.Applicative                 ((<$>))
import Control.Monad
import Data.Maybe
import System.Directory
import System.Exit
import System.Environment

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Implicit     hiding (Loud)
import System.Console.CmdArgs.Text

import Data.List                           (intercalate, nub)
import Data.Monoid

import           System.FilePath                     (dropFileName, isAbsolute,
                                                      takeDirectory, (</>))

import Language.Fixpoint.Config            hiding (Config, real, native, getOpts)
import Language.Fixpoint.Files
import Language.Fixpoint.Misc
import Language.Fixpoint.Names             (dropModuleNames)
import Language.Fixpoint.Types             hiding (Result)
import Language.Haskell.Liquid.Annotate
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Types       hiding (config, name, typ)
import Language.Haskell.Liquid.Errors

import Text.Parsec.Pos                     (newPos)
import Text.PrettyPrint.HughesPJ           hiding (Mode)


---------------------------------------------------------------------------------
-- Parsing Command Line----------------------------------------------------------
---------------------------------------------------------------------------------

config = cmdArgsMode $ Config {
   files
    = def &= typ "TARGET"
          &= args
          &= typFile

 , idirs
    = def &= typDir
          &= help "Paths to Spec Include Directory "

 , fullcheck
     = def
           &= help "Full Checking: check all binders (DEFAULT)"

 , diffcheck
    = def
          &= help "Incremental Checking: only check changed binders"

 , real
    = def
          &= help "Supports real number arithmetic"
 , native
    = def &= help "Use native (Haskell) fixpoint constraint solver"

 , binders
    = def &= help "Check a specific set of binders"

 , noVerify
    = def &= help "Skip verification"
          &= name "no-verify"

 , noPrune
    = def &= help "Disable prunning unsorted Predicates"
          &= name "no-prune-unsorted"

 , notermination
    = def &= help "Disable Termination Check"
          &= name "no-termination-check"

 , nowarnings
    = def &= help "Don't display warnings, only show errors"
          &= name "no-warnings"

 , trustinternals
    = def &= help "Trust all ghc auto generated code"
          &= name "trust-interals"

 , nocaseexpand
    = def &= help "Disable Termination Check"
          &= name "no-case-expand"
 , strata
    = def &= help "Enable Strata Analysis"

 , notruetypes
    = def &= help "Disable Trueing Top Level Types"
          &= name "no-true-types"

 , totality
    = def &= help "Check totality"

 , smtsolver
    = def &= help "Name of SMT-Solver"

 , noWriteIface
    = def &= explicit
          &= name "no-write-iface"
          &= help "Skip producing a .lqhi file for verified modules"

 , noCheckUnknown
    = def &= explicit
          &= name "no-check-unknown"
          &= help "Don't complain about specifications for unexported and unused values "

 , maxParams
    = 2   &= help "Restrict qualifier mining to those taking at most `m' parameters (2 by default)"

 , shortNames
    = def &= name "short-names"
          &= help "Print shortened names, i.e. drop all module qualifiers."

 , shortErrors
    = def &= name "short-errors"
          &= help "Don't show long error messages, just line numbers."

 , ghcOptions
    = def &= name "ghc-option"
          &= typ "OPTION"
          &= help "Pass this option to GHC"

 , cFiles
    = def &= name "c-files"
          &= typ "OPTION"
          &= help "Tell GHC to compile and link against these files"

 , noGhcPrimSpecs
    = def &= help "Turn off wired-in specifications for the `ghc-prim` package"
          &= name "no-ghc-prim-specs"

 , noBaseSpecs
    = def &= help "Turn off wired-in specifications for the `base` package"
          &= name "no-base-specs"

 } &= verbosity
   &= program "liquid"
   &= help    "Refinement Types for Haskell"
   &= summary copyright
   &= details [ "LiquidHaskell is a Refinement Type based verifier for Haskell"
              , ""
              , "To check a Haskell file foo.hs, type:"
              , "  liquid foo.hs "
              ]

getOpts :: [String] -> IO Config
getOpts args = do
  cfg0    <- envCfg
  cfg1    <- mkOpts =<< cmdArgsRun' config args
  cfg     <- fixConfig $ mconcat [cfg0, cfg1]
  whenNormal $ putStrLn copyright
  case smtsolver cfg of
    Just _  -> return cfg
    Nothing -> do smts <- mapM findSmtSolver [Z3, Cvc4, Mathsat]
                  case catMaybes smts of
                    (s:_) -> return (cfg {smtsolver = Just s})
                    _     -> exitWithPanic noSmtError
  where
    noSmtError = "LiquidHaskell requires an SMT Solver, i.e. z3, cvc4, or mathsat to be installed."

cmdArgsRun' :: Mode (CmdArgs a) -> [String] -> IO a
cmdArgsRun' mode args
  = case process mode args of
      Left err ->
        putStrLn (help err) >> exitFailure
      Right args ->
        cmdArgsApply args
    where
      help err = showText defaultWrap $ helpText [err] HelpFormatDefault mode

findSmtSolver :: SMTSolver -> IO (Maybe SMTSolver)
findSmtSolver smt = maybe Nothing (const $ Just smt) <$> findExecutable (show smt)

fixConfig :: Config -> IO Config
fixConfig cfg = do
  pwd <- getCurrentDirectory
  cfg <- canonicalizePaths pwd cfg
  return $ fixDiffCheck cfg

-- | Attempt to canonicalize all `FilePath's in the `Config' so we don't have
--   to worry about relative paths.
canonicalizePaths :: FilePath -> Config -> IO Config
canonicalizePaths pwd cfg = do
  tgt   <- canonicalizePath pwd
  isdir <- doesDirectoryExist tgt
  is    <- mapM (canonicalize tgt isdir) $ idirs cfg
  cs    <- mapM (canonicalize tgt isdir) $ cFiles cfg
  return $ cfg { idirs = is, cFiles = cs }

canonicalize :: FilePath -> Bool -> FilePath -> IO FilePath
canonicalize tgt isdir f
  | isAbsolute f = return f
  | isdir        = canonicalizePath (tgt </> f)
  | otherwise    = canonicalizePath (takeDirectory tgt </> f)

fixDiffCheck :: Config -> Config
fixDiffCheck cfg = cfg { diffcheck = diffcheck cfg && not (fullcheck cfg) }

envCfg = do so <- lookupEnv "LIQUIDHASKELL_OPTS"
            case so of
              Nothing -> return mempty
              Just s  -> parsePragma $ envLoc s
         where
            envLoc  = Loc l l
            l       = newPos "ENVIRONMENT" 0 0

copyright = "LiquidHaskell Copyright 2009-15 Regents of the University of California. All Rights Reserved.\n"

mkOpts :: Config -> IO Config
mkOpts cfg
  = do let files' = sortNub $ files cfg
       id0 <- getIncludeDir
       return  $ cfg { files = files' }
                     { idirs = (dropFileName <$> files') ++ [id0 </> gHC_VERSION, id0] ++ idirs cfg }
                              -- tests fail if you flip order of idirs'

---------------------------------------------------------------------------------------
-- | Updating options
---------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------
withPragmas :: Config -> FilePath -> [Located String] -> IO Config
---------------------------------------------------------------------------------------
withPragmas cfg fp ps = foldM withPragma cfg ps >>= canonicalizePaths fp

withPragma :: Config -> Located String -> IO Config
withPragma c s = (c `mappend`) <$> parsePragma s

parsePragma   :: Located String -> IO Config
parsePragma s = withArgs [val s] $ cmdArgsRun config



---------------------------------------------------------------------------------------
-- | Monoid instances for updating options
---------------------------------------------------------------------------------------


instance Monoid Config where
  mempty        = Config def def def def def def def def def def def def def def def def def def 2 def def def def def def def
  mappend c1 c2 = Config { files          = sortNub $ files c1   ++     files          c2
                         , idirs          = sortNub $ idirs c1   ++     idirs          c2
                         , fullcheck      = fullcheck c1         ||     fullcheck      c2
                         , real           = real      c1         ||     real           c2
                         , diffcheck      = diffcheck c1         ||     diffcheck      c2
                         , native         = native    c1         ||     native         c2
                         , binders        = sortNub $ binders c1 ++     binders        c2
                         , noVerify       = noVerify       c1    ||     noVerify       c2
                         , noWriteIface   = noWriteIface   c1    ||     noWriteIface   c2
                         , noCheckUnknown = noCheckUnknown c1    ||     noCheckUnknown c2
                         , notermination  = notermination  c1    ||     notermination  c2
                         , nowarnings     = nowarnings     c1    ||     nowarnings     c2
                         , trustinternals = trustinternals c1    ||     trustinternals c2
                         , nocaseexpand   = nocaseexpand   c1    ||     nocaseexpand   c2
                         , strata         = strata         c1    ||     strata         c2
                         , notruetypes    = notruetypes    c1    ||     notruetypes    c2
                         , totality       = totality       c1    ||     totality       c2
                         , noPrune        = noPrune        c1    ||     noPrune        c2
                         , maxParams      = maxParams      c1   `max`   maxParams      c2
                         , smtsolver      = smtsolver c1      `mappend` smtsolver      c2
                         , shortNames     = shortNames c1        ||     shortNames     c2
                         , shortErrors    = shortErrors c1       ||     shortErrors    c2
                         , ghcOptions     = ghcOptions c1        ++     ghcOptions     c2
                         , cFiles         = cFiles c1            ++     cFiles         c2
                         , noGhcPrimSpecs = noGhcPrimSpecs c1    ||     noGhcPrimSpecs c2
                         , noBaseSpecs    = noBaseSpecs c1       ||     noBaseSpecs    c2
                         }

instance Monoid SMTSolver where
  mempty        = def
  mappend s1 s2
    | s1 == s2  = s1
    | s2 == def = s1
    | otherwise = s2


------------------------------------------------------------------------
-- | Exit Function -----------------------------------------------------
------------------------------------------------------------------------

------------------------------------------------------------------------
exitWithResult :: Config -> FilePath -> Output Doc -> IO (Output Doc)
------------------------------------------------------------------------
exitWithResult cfg target out
  = do {-# SCC "annotate" #-} annotate cfg target out
       donePhase Loud "annotate"
       writeCheckVars $ o_vars  out
       writeResult cfg (colorResult r) r
       writeFile   (extFileName Result target) (showFix r)
       return $ out { o_result = r }
    where
       r         = o_result out `addErrors` o_errors out


writeCheckVars Nothing     = return ()
writeCheckVars (Just [])   = colorPhaseLn Loud "Checked Binders: None" ""
writeCheckVars (Just ns)   = colorPhaseLn Loud "Checked Binders:" "" >> forM_ ns (putStrLn . symbolString . dropModuleNames . symbol)

writeResult cfg c          = mapM_ (writeDoc c) . zip [0..] . resDocs tidy
  where
    tidy                   = if shortErrors cfg then Lossy else Full
    writeDoc c (i, d)      = writeBlock c i $ lines $ render d
    writeBlock _ _ []      = return ()
    writeBlock c 0 ss      = forM_ ss (colorPhaseLn c "")
    writeBlock _  _ ss     = forM_ ("\n" : ss) putStrLn

resDocs _ Safe             = [text "SAFE"]
resDocs k (Crash xs s)     = text ("ERROR: " ++ s) : pprManyOrdered k "" (errToFCrash <$> xs)
resDocs k (Unsafe xs)      = text "UNSAFE" : pprManyOrdered k "" (nub xs)
resDocs _ (UnknownError d) = [text $ "PANIC: Unexpected Error: " ++ d, reportUrl]

reportUrl              = text "Please submit a bug report at: https://github.com/ucsd-progsys/liquidhaskell"


addErrors r []             = r
addErrors Safe errs        = Unsafe errs
addErrors (Unsafe xs) errs = Unsafe (xs ++ errs)
addErrors r  _             = r
instance Fixpoint (FixResult Error) where
  toFix = vcat . resDocs Full
