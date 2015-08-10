{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE TupleSections             #-}

module Language.Haskell.Liquid.Pipeline.Ghc (
    -- * Extract All Information Needed for Verification
    getGhcInfo
  ) where

import GHC hiding (Target, desugarModule)

import Bag (bagToList)
import Class
import CoreMonad (liftIO)
import CoreSyn
import DataCon
import DynFlags
import ErrUtils
import Exception
import HscTypes hiding (Target)
import IdInfo
import InstEnv
import Panic
import Var

import Control.Applicative hiding (empty)
import Control.Monad (filterM, foldM, when, forM, forM_, liftM, void)

import Data.Monoid hiding ((<>))
import Data.List (find, intercalate, nub)
import Data.Maybe (catMaybes, maybeToList)

import qualified Data.HashSet        as S
import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Directory (removeFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath (replaceExtension, normalise)

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Files
import Language.Fixpoint.Misc
import Language.Fixpoint.Names
import Language.Fixpoint.Types hiding (Result, Expr)

import Language.Haskell.Liquid.Errors
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Spec
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.Visitors

import Language.Haskell.Liquid.Pipeline.ANFTransform

--------------------------------------------------------------------------------
-- Extract All Information Needed for Verification -----------------------------
--------------------------------------------------------------------------------

getGhcInfo :: Config -> GhcSpec -> FilePath -> ModSummary -> Ghc GhcInfo
getGhcInfo cfg scope hsFile summary = handleErrors $ do
  liftIO              $ cleanFiles hsFile
  summary'           <- updateDynFlags summary

  parsed             <- parseModule summary'
  let parsed'         = replaceModule (mkModuleName "LiquidHaskell") (mkModuleName "LiquidHaskell_") parsed
  typechecked        <- typecheckModule $ ignoreInline parsed'
  desugared          <- desugarModule typechecked
  loadModule desugared

  let modguts         = getGhcModGuts1 desugared
  hscEnv             <- getSession
  coreBinds          <- liftIO $ anormalize (not $ nocaseexpand cfg) hscEnv modguts

  let datacons        = [ dataConWorkId dc
                        | tc <- mgi_tcs modguts
                        , dc <- tyConDataCons tc
                        ]
  let impVs           = importVars  coreBinds ++ classCons (mgi_cls_inst modguts)
  let defVs           = definedVars coreBinds
  let useVs           = readVars    coreBinds
  let letVs           = letVars     coreBinds
  let derVs           = derivedVars coreBinds $ fmap (fmap is_dfun) $ mgi_cls_inst modguts

  setContext [IIModule $ moduleName $ ms_mod summary']
  spec               <- makeGhcSpec (mgi_exports modguts) typechecked (impVs ++ defVs) (mg_anns $ dm_core_module desugared) coreBinds scope

  hqualFiles         <- liftIO $ moduleHquals hsFile

  return              $ GI cfg hscEnv coreBinds derVs impVs (letVs ++ datacons) useVs hqualFiles [] [] spec

handleErrors :: Ghc a -> Ghc a
handleErrors act = act
  `gcatch` (\(e :: Error  ) -> pgmError $ show e)
  `gcatch` (\(e :: [Error]) -> pgmError $ intercalate "\n\n" $ map show e)


-- TODO: Rename, move?
cleanFiles :: FilePath -> IO ()
cleanFiles = createDirectoryIfMissing True . tempDirectory

-- TODO: Ensure our modifications to the DynFlags meet three goals:
--       (a) anything the module needs to compile, eg. extensions, persists;
--       (b) our setup is resilient enough that LH always has what it needs
--           under a variety of custom flags passed to GHC;
--       (c) LH's extra compilation step is as performant and light-weight as
--           possible.
updateDynFlags :: ModSummary -> Ghc ModSummary
updateDynFlags summary = do
  df@DynFlags{..} <- getSessionDynFlags
  let df' = df { pluginModNames     = filter (/= mkModuleName "LiquidHaskell.Plugin") pluginModNames
               , ghcMode            = CompManager
               , ghcLink            = LinkInMemory
               , optLevel           = 0
               , simplPhases        = 2
               , maxSimplIterations = 4
               , hscTarget          = HscInterpreted
               , profAuto           = ProfAutoCalls
               -- prevent GHC from printing anything
               , log_action         = \_ _ _ _ _ -> return ()
               -- , verbosity = 3
               } `xopt_set` Opt_DataKinds
                 `xopt_set` Opt_LiberalTypeSynonyms
                 `xopt_set` Opt_ConstraintKinds
                 `gopt_set` Opt_PIC
#if __GLASGOW_HASKELL__ >= 710
                 `gopt_set` Opt_Debug
#endif
  setSessionDynFlags $ df'
  return $ summary { ms_hspp_opts = df' }

replaceModule :: ModuleName -> ModuleName -> ParsedModule -> ParsedModule
replaceModule orig repl pm@(ParsedModule summary source _ _) =
  pm { pm_mod_summary =
         summary { ms_textual_imps = map (fmap go) $ ms_textual_imps summary
                 }
     , pm_parsed_source =
         fmap (\src -> src { hsmodImports = map (fmap go) $ hsmodImports src }) source
     }
  where
    go decl@(ImportDecl { ideclName = idn })
      = decl { ideclName = fmap go' idn }
    go' mod
      | mod == orig = repl
      | otherwise   = mod

--------------------------------------------------------------------------------
-- Extract Vars from Module ----------------------------------------------------
--------------------------------------------------------------------------------

classCons :: Maybe [ClsInst] -> [Id]
classCons Nothing   = []
classCons (Just cs) = concatMap (dataConImplicitIds . head . tyConDataCons . classTyCon . is_cls) cs

derivedVars :: CoreProgram -> Maybe [DFunId] -> [Id]
derivedVars cbs (Just fds) = concatMap (derivedVs cbs) fds
derivedVars _    Nothing    = []

derivedVs :: CoreProgram -> DFunId -> [Id]
derivedVs cbs fd = concatMap bindersOf cbf ++ deps
  where cbf            = filter f cbs

        f (NonRec x _) = eqFd x
        f (Rec xes   ) = any eqFd (fst <$> xes)
        eqFd x         = varName x == varName fd
        deps :: [Id]
        deps = concatMap dep $ (unfoldingInfo . idInfo <$> concatMap bindersOf cbf)

        dep (DFunUnfolding _ _ e)         = concatMap grapDep  e
        dep (CoreUnfolding {uf_tmpl = e}) = grapDep  e
        dep _                             = []

        grapDep :: CoreExpr -> [Id]
        grapDep e           = freeVars S.empty e

importVars :: CoreProgram -> [Id]
importVars = freeVars S.empty

definedVars :: CoreProgram -> [Id]
definedVars = concatMap defs
  where
    defs (NonRec x _) = [x]
    defs (Rec xes)    = map fst xes

--------------------------------------------------------------------------------
-- Build MGIModGuts for Module -------------------------------------------------
--------------------------------------------------------------------------------

getGhcModGuts1 :: DesugaredModule -> MGIModGuts
getGhcModGuts1 desugared =
   miModGuts (Just deriv) mod_guts
   where
     mod_guts = coreModule desugared
     deriv    = getDerivedDictionaries mod_guts

getDerivedDictionaries :: ModGuts -> [ClsInst]
getDerivedDictionaries = instEnvElts . mg_inst_env

--------------------------------------------------------------------------------
-- Extracting Qualifiers -------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Remove separate .hquals files
moduleHquals :: FilePath -> IO [FilePath]
moduleHquals target
  = do hqs <- filterM doesFileExist [extFileName Hquals target]
       whenLoud $ putStrLn $ "Reading Qualifiers From: " ++ show hqs
       return hqs

