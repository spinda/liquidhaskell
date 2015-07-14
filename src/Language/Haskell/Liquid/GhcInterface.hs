{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Language.Haskell.Liquid.GhcInterface (

  -- * extract all information needed for verification
    getGhcInfo

  ) where
import IdInfo
import InstEnv
import Bag (bagToList)
import ErrUtils
import GHC hiding (Target, desugarModule)
import Text.PrettyPrint.HughesPJ
import HscTypes hiding (Target)
import CoreSyn

import Class
import Var
import CoreMonad    (liftIO)
import DataCon
import qualified Control.Exception as Ex

import GHC.Paths (libdir)
import System.FilePath ( replaceExtension, normalise)

import DynFlags
import Control.Monad (filterM, foldM, when, forM, forM_, liftM, void)
import Control.Applicative  hiding (empty)
import Data.Bifunctor
import Data.Monoid hiding ((<>))
import Data.List (find, nub)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.HashSet        as S
import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Directory (removeFile, createDirectoryIfMissing, doesFileExist)
import Language.Fixpoint.Misc
import Language.Fixpoint.Names
import Language.Fixpoint.Files
import Language.Fixpoint.Types hiding (Result, Expr)

import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Errors
import Language.Haskell.Liquid.ANFTransform
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Spec
import Language.Haskell.Liquid.Visitors
import Language.Haskell.Liquid.CmdLine (withCabal, withPragmas)

import qualified Language.Haskell.Liquid.Measure as Ms

getGhcInfo :: Config -> GhcSpec -> FilePath -> ModSummary -> Ghc GhcInfo
getGhcInfo cfg scope hsFile summary = do
  liftIO              $ cleanFiles hsFile
  summary'           <- updateDynFlags cfg summary

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
  spec               <- makeGhcSpec cfg (mgi_exports modguts) typechecked (impVs ++ defVs) (mg_anns $ dm_core_module desugared) coreBinds scope

  let paths           = idirs cfg
  liftIO              $ whenLoud $ putStrLn ("paths = " ++ show paths)
  hqualFiles         <- moduleHquals modguts paths hsFile

  return              $ GI hscEnv coreBinds derVs impVs (letVs ++ datacons) useVs hqualFiles [] [] spec


-- TODO: Ensure our modifications to the DynFlags meet three goals:
--       (a) anything the module needs to compile, eg. extensions, persists;
--       (b) our setup is resilient enough that LH always has what it needs
--           under a variety of custom flags passed to GHC;
--       (c) LH's extra compilation step is as performant and light-weight as
--           possible.
updateDynFlags :: Config -> ModSummary -> Ghc ModSummary
updateDynFlags cfg summary = do
  df <- getSessionDynFlags
  let df' = df { importPaths        = idirs cfg ++ importPaths df
               , libraryPaths       = idirs cfg ++ libraryPaths df
               , includePaths       = idirs cfg ++ includePaths df
               , pluginModNames     = filter (/= mkModuleName "LiquidHaskell.Plugin") $ pluginModNames df
               , ghcMode            = CompManager
               , ghcLink            = NoLink
               , optLevel           = 0
               , simplPhases        = 2
               , maxSimplIterations = 4
               , hscTarget          = HscNothing
               , profAuto           = ProfAutoCalls
               -- prevent GHC from printing anything
               , log_action         = \_ _ _ _ _ -> return ()
               -- , verbosity = 3
               } `xopt_set` Opt_DataKinds
                 `xopt_set` Opt_LiberalTypeSynonyms
                 `gopt_set` Opt_PIC
#if __GLASGOW_HASKELL__ >= 710
                 `gopt_set` Opt_Debug
#endif
  (df'',_,_) <- parseDynamicFlags df' (map noLoc $ ghcOptions cfg)
  setSessionDynFlags $ df''
  return $ summary { ms_hspp_opts = df'' }


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

mgi_namestring = moduleNameString . moduleName . mgi_module

importVars            = freeVars S.empty

definedVars           = concatMap defs
  where
    defs (NonRec x _) = [x]
    defs (Rec xes)    = map fst xes


------------------------------------------------------------------
-- | Extracting CoreBindings From File ---------------------------
------------------------------------------------------------------

getGhcModGuts1 :: DesugaredModule -> MGIModGuts
getGhcModGuts1 desugared =
   miModGuts (Just deriv) mod_guts
   where
     mod_guts = coreModule desugared
     deriv    = getDerivedDictionaries mod_guts

getDerivedDictionaries cm = instEnvElts $ mg_inst_env cm

cleanFiles :: FilePath -> IO ()
cleanFiles fn
  = do forM_ bins (tryIgnore "delete binaries" . removeFileIfExists)
       tryIgnore "create temp directory" $ createDirectoryIfMissing False dir
    where
       bins = replaceExtension fn <$> [] --"hi", "o"]
       dir  = tempDirectory fn


removeFileIfExists f = doesFileExist f >>= (`when` removeFile f)

--------------------------------------------------------------------------------
-- | Extracting Qualifiers -----------------------------------------------------
--------------------------------------------------------------------------------

moduleHquals mg paths target
  = do hqs <- liftIO   $ filterM doesFileExist [extFileName Hquals target]
       liftIO $ whenLoud $ putStrLn $ "Reading Qualifiers From: " ++ show hqs
       return hqs

--------------------------------------------------------------------------------
-- | Extracting Specifications (Measures + Assumptions) ------------------------
--------------------------------------------------------------------------------

patErrorName    = "PatErr"
realSpecName    = "Real"
notRealSpecName = "NotReal"

getPatSpec paths totalitycheck
  | totalitycheck
  = (map (patErrorName, )) . maybeToList <$> moduleFile paths patErrorName Spec
  | otherwise
  = return []

getRealSpec paths freal
  | freal
  = (map (realSpecName, )) . maybeToList <$> moduleFile paths realSpecName Spec
  | otherwise
  = (map (notRealSpecName, )) . maybeToList <$> moduleFile paths notRealSpecName Spec

moduleImports :: GhcMonad m => [Ext] -> [FilePath] -> [String] -> m [(String, FilePath)]
moduleImports exts paths names
  = liftM concat $ forM names $ \name -> do
      map (name,) . catMaybes <$> mapM (moduleFile paths name) exts

moduleFile :: GhcMonad m => [FilePath] -> String -> Ext -> m (Maybe FilePath)
moduleFile paths name ext
  | ext `elem` [Hs, LHs]
  = do mg <- getModuleGraph
       case find ((==name) . moduleNameString . ms_mod_name) mg of
         Nothing -> liftIO $ getFileInDirs (extModuleName name ext) paths
         Just ms -> return $ normalise <$> ml_hs_file (ms_location ms)
  | otherwise
  = liftIO $ getFileInDirs (extModuleName name ext) paths

specIncludes :: GhcMonad m => Ext -> [FilePath] -> [FilePath] -> m [FilePath]
specIncludes ext paths reqs
  = do let libFile  = extFileNameR ext $ symbolString preludeName
       let incFiles = catMaybes $ reqFile ext <$> reqs
       liftIO $ forM (libFile : incFiles) $ \f -> do
         mfile <- getFileInDirs f paths
         case mfile of
           Just file -> return file
           Nothing -> errorstar $ "cannot find " ++ f ++ " in " ++ show paths

reqFile ext s
  | isExtFile ext s
  = Just s
  | otherwise
  = Nothing





instance PPrint GhcSpec where
  pprint spec =  (text "******* Target Variables ********************")
              $$ (pprint $ tgtVars spec)
              $$ (text "******* Type Signatures *********************")
              $$ (pprintLongList $ tySigs spec)
              $$ (text "******* Type Synonyms ***********************")
              $$ (pprintLongList $ M.toList $ rtEnv spec)
              $$ (text "******* FTycon Embeds ***********************")
              $$ (pprintLongList $ map (second fTyconSymbol) $ M.toList $ tcEmbeds spec)
              $$ (text "******* Assumed Type Signatures *************")
              $$ (pprintLongList $ asmSigs spec)
              $$ (text "******* DataCon Specifications (Measure) ****")
              $$ (pprintLongList $ ctors spec)
              $$ (text "******* Measure Specifications **************")
              $$ (pprintLongList $ meas spec)

instance PPrint GhcInfo where
  pprint info =   (text "*************** Imports *********************")
              $+$ (intersperse comma $ text <$> imports info)
              $+$ (text "*************** Includes ********************")
              $+$ (intersperse comma $ text <$> includes info)
              $+$ (text "*************** Imported Variables **********")
              $+$ (pprDoc $ impVars info)
              $+$ (text "*************** Defined Variables ***********")
              $+$ (pprDoc $ defVars info)
              $+$ (text "*************** Specification ***************")
              $+$ (pprint $ spec info)
              $+$ (text "*************** Core Bindings ***************")
              $+$ (pprint $ cbs info)

instance Show GhcInfo where
  show = showpp

instance PPrint [CoreBind] where
  pprint = pprDoc . tidyCBs

instance PPrint TargetVars where
  pprint AllVars   = text "All Variables"
  pprint (Only vs) = text "Only Variables: " <+> pprint vs

------------------------------------------------------------------------
-- Dealing With Errors -------------------------------------------------
------------------------------------------------------------------------
-- | Convert a GHC error into one of ours
instance Result SourceError where
  result = (`Crash` "Invalid Source")
         . concatMap errMsgErrors
         . bagToList
         . srcErrorMessages

errMsgErrors e = [ ErrGhc (errMsgSpan e) (pprint e)]
