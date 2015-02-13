{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances      #-} 
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Language.Haskell.Liquid.GhcInterface (
    verifyTargets

  -- * visitors 
  , CBVisitable (..) 
  ) where

--------------------------------------------------------------------------------

import Finder (FindResult(..), findImportedModule)
import IdInfo
import InstEnv
import Bag (bagToList)
import ErrUtils
import GHC hiding (Target)
import NameSet
import DriverPhases (Phase(..))
import DriverPipeline (compileFile)
import Text.PrettyPrint.HughesPJ
import HscTypes hiding (Target)
import Literal
import CoreSyn

import Var
import DataCon
import Language.Haskell.Liquid.Desugar.HscMain (hscDesugarWithLoc) 
import qualified Control.Exception as Ex

import GHC.Paths (libdir)
import System.FilePath ( replaceExtension, normalise)

import DynFlags
import Control.Monad.RWS
import Control.Monad.State
import Control.DeepSeq
import Control.Applicative  hiding (empty)
import Data.Monoid hiding ((<>))
import Data.List (foldl', find, (\\), delete, nub)
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)

import qualified Data.HashSet        as S
import qualified Data.HashMap.Strict as M
import qualified Data.Graph          as G
import qualified Data.Tree           as T
  
import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Directory (removeFile, createDirectory, doesFileExist, getModificationTime)
import System.Exit (exitWith)

import Data.Time.Clock.POSIX

import Language.Fixpoint.Interface (resultExit)
import Language.Fixpoint.Misc
import Language.Fixpoint.Types hiding (Expr) 

import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.ANFTransform
import Language.Haskell.Liquid.Bare
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Interface
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.PrettyPrint

import Language.Haskell.Liquid.CmdLine (withPragmas)
import Language.Haskell.Liquid.Parse

import Language.Fixpoint.Names
import Language.Fixpoint.Files

import qualified Language.Haskell.Liquid.Measure as Ms

--------------------------------------------------------------------------------

-- TOOD: Pull exit/exception(?) code to outer layer?

verifyTargets :: Config -> [FilePath] -> (FilePath -> GhcInfo -> IO (Output Doc)) -> IO ()
verifyTargets cfg targets verify
  = do (runGhc (Just libdir) (verifyTargets' cfg targets verify))
         `Ex.catch` (\(e :: SourceError) -> handle e)
         `Ex.catch` (\(e :: Error)       -> handle e)
         `Ex.catch` (\(e :: [Error])     -> handle e)
  where
    -- TODO: Handle this better
    handle e
      = do putStrLn $ show e
           exitWith $ resultExit $ result e

--------------------------------------------------------------------------------

verifyTargets' :: Config -> [FilePath] -> (FilePath -> GhcInfo -> IO (Output Doc)) -> Ghc ()
verifyTargets' cfg targets verify
  = do configureGhc cfg
       logicMap <- makeLogicMap

       depGraph <- buildMakeGraph cfg targets
       plan     <- liftIO $ buildVerificationPlan (idirs cfg) depGraph
       executeVerificationPlan cfg logicMap verify plan


configureGhc :: Config -> Ghc ()
configureGhc cfg
  = do updateDynFlags cfg
       compileCFiles cfg

updateDynFlags :: Config -> Ghc ()
updateDynFlags cfg
  = do df <- getSessionDynFlags
       let df' = df { importPaths  = idirs cfg ++ importPaths  df
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
       -- TODO: Report left-over arguments and warnings returned?
       (df'', _, _) <- parseDynamicFlags df' (map noLoc $ ghcOptions cfg)
       setSessionDynFlags df''
       return ()

compileCFiles :: Config -> Ghc ()
compileCFiles cfg
  = do hsc <- getSession
       os  <- mapM (\x -> liftIO $ compileFile hsc StopLn (x,Nothing)) (nub $ cFiles cfg)
       df  <- getSessionDynFlags
       setSessionDynFlags $ df { ldInputs = map (FileOption "") os ++ ldInputs df }
       return ()


makeLogicMap :: Ghc LogicMap
makeLogicMap 
  = do lg    <- liftIO $ getCoreToLogicPath
       lspec <- liftIO $ readFile lg
       either Ex.throw return $ parseSymbolToLogic lg lspec

--------------------------------------------------------------------------------

type SpecData = Either GoodSpec BadSpec

data GoodSpec = GHsSpec
                  { gs_name    :: ModuleName
                  , gs_file    :: FilePath
                  , gs_imports :: [ModuleName]
                  , gs_spec    :: BInterface
                  , gs_hquals  :: S.HashSet FilePath
                  , gs_summary :: ModSummary
                  }
              | GSpecFile
                  { gs_name    :: ModuleName
                  , gs_file    :: FilePath
                  , gs_imports :: [ModuleName]
                  , gs_spec    :: BInterface
                  , gs_hquals  :: S.HashSet FilePath
                  }
              | GNoSpec
                  { gs_name    :: ModuleName
                  , gs_hquals  :: S.HashSet FilePath
                  }

data BadSpec = BHsSpec
                 { bs_name     :: ModuleName
                 , bs_file     :: FilePath
                 , bs_imports  :: [ModuleName]
                 , bs_hspec    :: Ms.BareSpec
                 , bs_ispec    :: Maybe Ms.BareSpec
                 , bs_hquals   :: S.HashSet FilePath
                 , bs_summary  :: ModSummary
                 }
             | BSpecFile
                 { bs_name     :: ModuleName
                 , bs_file     :: FilePath
                 , bs_imports  :: [ModuleName]
                 , bs_spec     :: Ms.BareSpec
                 , bs_hquals   :: S.HashSet FilePath
                 }


type Graph key node = (G.Graph, G.Vertex -> (node, key, [key]), key -> Maybe G.Vertex)
type DepGraph       = Graph ModuleName SpecData

type GoodSpecs = M.HashMap ModuleName GoodSpec
type BadSpecs  = [([ModuleName], BadSpec)]

--------------------------------------------------------------------------------

buildMakeGraph :: Config -> [FilePath] -> Ghc DepGraph
buildMakeGraph cfg targets
  = do hsGraph   <- analyzeTargets (idirs cfg) targets
       fullGraph <- descendImportGraph (idirs cfg) hsGraph
       case mapMaybe checkCyclic (G.stronglyConnCompR fullGraph) of
         [] ->
           return $ G.graphFromEdges fullGraph
         errs ->
           Ex.throw errs
  where
    checkCyclic :: G.SCC (SpecData, ModuleName, [ModuleName]) -> Maybe Error
    checkCyclic (G.AcyclicSCC _)
      = Nothing
    checkCyclic (G.CyclicSCC vs)
      = Just $ ErrImportCycle { pos    = noSrcSpan
                              , icycle = map snd3 vs
                              }


scanSpec :: FilePath -> IO (Either BInterface Ms.BareSpec)
scanSpec specFile
  = maybe parse validate =<< loadInterface specFile
  where
    parse
      = do (_, spec) <- parseSpec specFile
           return $ Right spec
    validate intr
      -- TODO: Move validation to Interface module(?)
      = do checksum <- computeChecksum specFile
           if checksum == intr_checksum intr
              then return $ Left intr
              else parse

computeChecksum :: FilePath -> IO Integer
computeChecksum file
  = (round . utcTimeToPOSIXSeconds) <$> getModificationTime file


analyzeTargets :: [FilePath] -> [FilePath] -> Ghc [(SpecData, ModuleName, [ModuleName])]
analyzeTargets idirs targets
  = do mapM_ (addTarget <=< flip guessTarget Nothing) targets
       summaries <- depanal [] False
       load LoadAllTargets
       mapM (analyzeSummary idirs) summaries

analyzeSummary :: [FilePath] -> ModSummary -> Ghc (SpecData, ModuleName, [ModuleName])
analyzeSummary idirs summary
  = do file       <- getSourcePath summary
       (imps, sd) <- process file =<< (liftIO $ scanSpec file)
       return (sd, name, imps)
  where
    name
      = moduleName $ ms_mod summary

    process file (Left intr)
      = do -- TODO: Move hquals into lqhi
           hquals <- S.fromList <$> moduleHquals idirs (Just file) name [] (intr_includes intr)
           imps   <- getImports name
           return
             ( imps
             , Left $ GHsSpec
                 { gs_name    = name
                 , gs_file    = file
                 , gs_imports = imps
                 , gs_spec    = intr
                 , gs_hquals  = hquals
                 , gs_summary = summary
                 }
             )
    process file (Right hspec)
      = do hquals <- S.fromList <$> moduleHquals idirs (Just file) name [] (Ms.includes hspec)
           ispec  <- liftIO $ findParseSpecFile idirs name
           imps   <- getImports name
           return
             ( imps
             , Right $ BHsSpec
                 { bs_name    = name
                 , bs_file    = file
                 , bs_imports = imps
                 , bs_hspec   = hspec
                 , bs_ispec   = ispec
                 , bs_hquals  = hquals
                 , bs_summary = summary
                 }
             )

getSourcePath :: ModSummary -> Ghc FilePath
getSourcePath summary
  = maybe (Ex.throw err) return (ml_hs_file $ ms_location summary)
  where
    err :: Error
    err
      = ErrMissingSpec { pos     = noSrcSpan
                       , missing = ms_mod summary
                       }


descendImportGraph :: [FilePath] -> [(SpecData, ModuleName, [ModuleName])] -> Ghc [(SpecData, ModuleName, [ModuleName])]
descendImportGraph idirs graph
  = ((graph ++) . snd) <$> execRWST (descendImportGraph' graph) idirs S.empty

-- TODO: Base things on 'Module' instead of 'ModuleName'?
-- TODO: Clean up this signature
descendImportGraph' :: [(SpecData, ModuleName, [ModuleName])] -> RWST [FilePath] [(SpecData, ModuleName, [ModuleName])] (S.HashSet ModuleName) Ghc ()
descendImportGraph' []
  = return ()
descendImportGraph' lastBatch
  = do idirs <- ask
       seen  <- get

       let seen'   = S.union seen $ S.fromList $ map snd3 lastBatch
           imports = S.fromList $ concatMap thd3 lastBatch
           new     = S.difference imports seen'

       batch <- lift $ mapM (analyzeSpecImport idirs) (S.toList new)

       put seen'
       tell batch
       descendImportGraph' batch

-- TODO: Clean up this function!
analyzeSpecImport :: [FilePath] -> ModuleName -> Ghc (SpecData, ModuleName, [ModuleName])
analyzeSpecImport idirs name
  = do file <- liftIO $ findSpecFile idirs name
       maybe noSpec scan file
  where
    noSpec
      = do hquals <- S.fromList <$> moduleHquals idirs Nothing name [] []
           return $
             ( Left $ GNoSpec
                 { gs_name   = name
                 , gs_hquals = hquals
                 }
             , name
             , []
             )
    scan file
      = do scanned <- liftIO $ scanSpec file
           case scanned of
             Left intr ->
               do let imps = map moduleNameString (intr_imports intr)
                  hquals <- S.fromList <$> moduleHquals idirs (Just file) name imps (intr_includes intr)
                  return $
                    ( Left $ GSpecFile
                        { gs_name    = name
                        , gs_file    = file
                        , gs_imports = intr_imports intr
                        , gs_spec    = intr
                        , gs_hquals  = hquals
                        }
                    , name
                    , intr_imports intr
                    )
             Right spec ->
               do imports <- getImports name
                  hquals  <- S.fromList <$> moduleHquals idirs (Just file) name (map moduleNameString imports) (Ms.includes spec)
                  return $
                    ( Right $ BSpecFile
                        { bs_name    = name
                        , bs_file    = file
                        , bs_imports = imports
                        , bs_spec    = spec
                        , bs_hquals  = hquals
                        }
                    , name
                    , imports
                    )


findSpecFile :: [FilePath] -> ModuleName -> IO (Maybe FilePath)
findSpecFile idirs name
  = getFileInDirs (extModuleName (moduleNameString name) Spec) idirs

findParseSpecFile :: [FilePath] -> ModuleName -> IO (Maybe Ms.BareSpec)
findParseSpecFile idirs name
  = do file <- findSpecFile idirs name
       case file of
         Nothing ->
           return Nothing
         Just file' ->
           (Just . snd) <$> parseSpec file'

--------------------------------------------------------------------------------

buildVerificationPlan :: [FilePath] -> DepGraph -> IO (BadSpecs, {- GoodSpecs -} M.HashMap ModuleName SpecData)
buildVerificationPlan idirs graph@(digraph, resolveVertex, _)
  = do (badSpecs, goodSpecs) <- runStateT (mapM (analyzeNode idirs graph) sorted) mempty
       return (catMaybes badSpecs, goodSpecs)
  where
    sorted
      = map resolveVertex $ reverse $ G.topSort digraph

analyzeNode :: [FilePath]
            -> DepGraph
            -> (SpecData, ModuleName, [ModuleName])
            -> StateT {- GoodSpecs -} (M.HashMap ModuleName SpecData) IO (Maybe ([ModuleName], BadSpec))
analyzeNode idirs graph (sd, name, _)
  = go sd
  where
    go :: Either GoodSpec BadSpec -> StateT (M.HashMap ModuleName SpecData) IO (Maybe ([ModuleName], BadSpec))
    go (Right badSpec)
      = return $ Just
          ( getAllDeps graph (bs_name badSpec)
          , badSpec
          )

    go (Left gs@(GNoSpec {}))
      = do modify $ M.insert name (Left gs)
           return Nothing

    go (Left goodSpec)
      = do env <- get
           let deps = getAllDeps graph (gs_name goodSpec)
           if null deps || all (isJust . flip M.lookup env) deps
              then do {- modify $ M.insert name goodSpec
                         return Nothing
                       -}
                      badSpec <- liftIO $ parse goodSpec
                      modify $ M.insert name (Right badSpec)
                      return Nothing
              else do badSpec <- liftIO $ parse goodSpec
                      return $ Just (deps, badSpec)

    parse gs@(GHsSpec {})
      = do (_, hspec) <- parseSpec $ gs_file gs
           ispec      <- findParseSpecFile idirs (gs_name gs)
           return $ BHsSpec
              { bs_name    = gs_name gs
              , bs_file    = gs_file gs
              , bs_imports = gs_imports gs
              , bs_hspec   = hspec
              , bs_ispec   = ispec
              , bs_hquals  = gs_hquals gs
              , bs_summary = gs_summary gs
              }
    parse gs@(GSpecFile {})
      = do (_, spec) <- parseSpec $ gs_file gs
           return $ BSpecFile
             { bs_name    = gs_name gs
             , bs_file    = gs_file gs
             , bs_imports = gs_imports gs
             , bs_spec    = spec
             , bs_hquals  = gs_hquals gs
             }
    parse (GNoSpec {})
      = error "parse GNoSpec"

getAllDeps :: DepGraph -> ModuleName -> [ModuleName]
getAllDeps (digraph, resolveVertex, lookupVertex) name
  = reverse $ map (snd3 . resolveVertex) $ filter (/= vertex) $ concatMap T.flatten $ G.dfs digraph [vertex]
  where
    vertex = safeFromJust "getAllDeps : fromJust" $ lookupVertex name

--------------------------------------------------------------------------------

executeVerificationPlan :: Config
                        -> LogicMap
                        -> (FilePath -> GhcInfo -> IO (Output Doc))
                        -> (BadSpecs, {- GoodSpecs -} M.HashMap ModuleName SpecData)
                        -> Ghc ()
executeVerificationPlan cfg logicMap verify (badSpecs, goodSpecs)
  = evalStateT (mapM_ (verifyModule cfg logicMap verify) badSpecs) goodSpecs

verifyModule :: Config
             -> LogicMap
             -> (FilePath -> GhcInfo -> IO (Output Doc))
             -> ([ModuleName], BadSpec)
             -> StateT {- GoodSpecs -} (M.HashMap ModuleName SpecData) Ghc ()
verifyModule cfg logicMap verify
  = go
  where
    go (deps, bhs@(BHsSpec name file imports bareSpec _ hquals summary))
      = do liftIO $ putStrLn $ "=== " ++ showpp name ++ " ==="

           let mod   = ms_mod summary
               name  = moduleName mod
               name' = ModName Target name

           --liftIO $ cleanFiles file
           --lift   $ load $ LoadUpTo name

           cfg' <- liftIO $ withPragmas cfg file (Ms.pragmas bareSpec)

           impSpecs  <- map noTerm <$> retrieveSpecs deps
           impHquals <- retrieveHquals deps

           phantomImports <- liftIO $ getPhantomImports cfg'

           let allSpecs  = (name', bareSpec) : phantomImports ++ impSpecs
               allHquals = S.toList $ S.union hquals impHquals

           liftIO $ putStrLn $ show $ map fst impSpecs

           let context = (IIModule name) : map (IIDecl . qualImportDecl) deps
           lift $ setContext context

           modguts <- lift     $ getGhcModGuts1 summary
           hscEnv  <- lift     $ getSession
           exports <- lift     $ getExports mod
           coreBinds <- liftIO $ anormalize (not $ nocaseexpand cfg') hscEnv modguts

           let impVs = importVars  coreBinds
               defVs = definedVars coreBinds
               useVs = readVars    coreBinds
               letVs = letVars     coreBinds
               derVs = derivedVars coreBinds $ mgi_is_dfun modguts

           ghcSpec <- liftIO $ makeGhcSpec cfg' name' coreBinds (impVs ++ defVs) letVs exports hscEnv logicMap allSpecs

           let ghcInfo = GI hscEnv coreBinds derVs impVs letVs useVs allHquals ghcSpec

           out <- liftIO $ verify file ghcInfo
           unless (o_result out == Safe)
                  (Ex.throw $ PhaseFailed "liquid" $ resultExit $ o_result out)

           -- TODO: Include ispec in checksum
           -- TODO: Encode dependency checksums in .lqhi file!
           -- TODO: Move checksum code to Interface module
           checksum <- liftIO $ computeChecksum file
           liftIO $ saveInterface file $ packInterface $ buildInterface checksum bareSpec ghcSpec imports

           modify $ M.insert name (Right bhs)

    go (deps, bsf@(BSpecFile name file imports bareSpec hquals))
      = do liftIO $ putStrLn $ "=== " ++ showpp name ++ " ==="

           mod <- lift $ findModule name Nothing

           let name' = ModName Target name

           cfg' <- liftIO $ withPragmas cfg file (Ms.pragmas bareSpec)

           impSpecs  <- map noTerm <$> retrieveSpecs deps
           impHquals <- retrieveHquals deps

           phantomImports <- liftIO $ getPhantomImports cfg'

           liftIO $ putStrLn $ show $ map fst impSpecs

           let allSpecs  = (name', bareSpec) : phantomImports ++ impSpecs
               allHquals = S.toList $ S.union hquals impHquals 

           let context = (IIDecl $ simpleImportDecl name) : map (IIDecl . qualImportDecl) deps
           lift $ setContext context

           hscEnv  <- lift   $ getSession
           exports <- lift   $ getExports mod

           ghcSpec <- liftIO $ makeGhcSpec cfg' name' [] [] [] exports hscEnv logicMap allSpecs

           --let ghcInfo = GI hscEnv [] [] [] [] [] allHquals ghcSpec

           -- TODO: Encode dependency checksums in .lqhi file!
           -- TODO: Move checksum code to Interface module
           checksum <- liftIO $ computeChecksum (bs_file bsf)
           liftIO $ saveInterface file $ packInterface $ buildInterface checksum bareSpec ghcSpec imports

           modify $ M.insert (bs_name bsf) (Right bsf)

    noTerm (m, spec)
      = (m, spec { Ms.decr=mempty, Ms.lazy=mempty, Ms.termexprs=mempty })

retrieveSpecs :: [ModuleName]
              -> StateT {- GoodSpecs -} (M.HashMap ModuleName SpecData) Ghc [(ModName, Ms.BareSpec)]
retrieveSpecs deps
  = do env <- get
       return $ concatMap (go env) deps
  where
      -- TOOD: Put out a proper error on missing dep
    go env dep
      = case safeFromJust "retrieveSpecs : fromJust" $ M.lookup dep env of
          Left (GNoSpec {}) ->
            []
          Right bhs@(BHsSpec {}) ->
            [ ( ModName SrcImport dep
              , bs_hspec bhs
              )
            ] ++ (maybeToList $ (ModName SrcImport dep, ) <$> bs_ispec bhs)
          Right bsf@(BSpecFile {}) ->
            [ ( ModName SpecImport dep
              , bs_spec bsf
              )
            ]
          _ ->
            error "TODO: Fix up retrieveSpecs when possible"

retrieveHquals :: [ModuleName]
              -> StateT {- GoodSpecs -} (M.HashMap ModuleName SpecData) Ghc (S.HashSet FilePath)
retrieveHquals deps
  = do env <- get
       return $ S.unions $ map (go env) deps
  where
    go env dep
      = case safeFromJust "retrieveHquals : fromJust" $ M.lookup dep env of
          Left gs ->
            gs_hquals gs
          Right bs ->
            bs_hquals bs


-- TODO: Temporary hack until proper .lqhi support is in
getPhantomImports :: Config -> IO [(ModName, Ms.BareSpec)]
getPhantomImports cfg
  = mapM go (patSpec ++ realSpec)
  where
    patSpec
      | totality cfg = ["PatErr"]
      | otherwise    = []
    realSpec
      | real cfg     = ["Real"]
      | otherwise    = ["NotReal"]

    go name
      = do let specFile = extModuleName name Spec
           specPath <- safeFromJust "getPhantomImports : fromJust" <$> getFileInDirs specFile (idirs cfg)
           parseSpec specPath

getImports :: ModuleName -> Ghc [ModuleName]
getImports name
  = do mod   <- findModule name Nothing
       minfo <- getModuleInfo mod >>= maybe (Ex.throw err) return
       let iface  = safeFromJust "getImports : modInfoIface : fromJust " $ modInfoIface minfo
           usages = mi_usages iface
       filterM isExposed $ mapMaybe ofUsage usages
  where
    ofUsage pkg@(UsagePackageModule {})
      = Just $ moduleName $ usg_mod pkg
    ofUsage home@(UsageHomeModule {})
      = Just $ usg_mod_name home
    ofUsage (UsageFile {})
      = Nothing

    -- TODO: Find a better way to detect hidden modules? (or get rid of the need for this check)
    isExposed name
      = do hscEnv <- getSession
           result <- liftIO $ findImportedModule hscEnv name Nothing
           case result of
             err@(NotFound {})
               | not $ null (fr_mods_hidden err ++ fr_pkgs_hidden err) ->
                 return False
               | otherwise ->
                 return True
             _ ->
               return True

    err :: Error
    err
      = error "TODO: GhcInterface.getImports error"

getExports :: Module -> Ghc NameSet
getExports mod
  = do minfo <- getModuleInfo mod >>= maybe (Ex.throw err) return
       return $ mkNameSet $ modInfoExports minfo
  where
    err :: Error
    err
      = error "TODO: GhcInterface.getExports error"

--------------------------------------------------------------------------------

importVars :: CoreProgram -> [Var]
importVars = freeVars S.empty 

definedVars :: CoreProgram -> [Var]
definedVars = concatMap defs 
  where 
    defs (NonRec x _) = [x]
    defs (Rec xes)    = map fst xes

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

--------------------------------------------------------------------------------

------------------------------------------------------------------
-- | Extracting CoreBindings From File ---------------------------
------------------------------------------------------------------
getGhcModGuts1 :: ModSummary -> Ghc MGIModGuts
getGhcModGuts1 summary = do
   mod_p      <- parseModule summary
   mod_guts   <- coreModule <$> (desugarModuleWithLoc =<< typecheckModule (ignoreInline mod_p))
   let deriv   = getDerivedDictionaries mod_guts 
   return     $! (miModGuts (Just deriv) mod_guts)

getDerivedDictionaries cm = is_dfun <$> (instEnvElts $ mg_inst_env cm)

cleanFiles :: FilePath -> IO ()
cleanFiles fn 
  = do forM_ bins (tryIgnore "delete binaries" . removeFileIfExists)
       tryIgnore "create temp directory" $ createDirectory dir 
    where 
       bins = replaceExtension fn <$> ["hi", "o"]
       dir  = tempDirectory fn


removeFileIfExists f = doesFileExist f >>= (`when` removeFile f)

--------------------------------------------------------------------------------
-- | Desugaring (Taken from GHC, modified to hold onto Loc in Ticks) -----------
--------------------------------------------------------------------------------

desugarModuleWithLoc :: TypecheckedModule -> Ghc DesugaredModule
desugarModuleWithLoc tcm = do
  let ms = pm_mod_summary $ tm_parsed_module tcm 
  -- let ms = modSummary tcm
  let (tcg, _) = tm_internals_ tcm
  hsc_env <- getSession
  let hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }
  guts <- liftIO $ hscDesugarWithLoc hsc_env_tmp ms tcg
  return $ DesugaredModule { dm_typechecked_module = tcm, dm_core_module = guts }

--------------------------------------------------------------------------------
-- | Extracting Qualifiers -----------------------------------------------------
--------------------------------------------------------------------------------

moduleHquals paths target name imps incs 
  = do hqs   <- specIncludes Hquals paths incs 
       hqs'  <- moduleImports [Hquals] paths (moduleNameString name : imps)
       hqs'' <- liftIO   $ filterM doesFileExist $ maybeToList $ extFileName Hquals <$> target
       let rv = sortNub  $ hqs'' ++ hqs ++ (snd <$> hqs')
       liftIO $ whenLoud $ putStrLn $ "Reading Qualifiers From: " ++ show rv 
       return rv

--------------------------------------------------------------------------------
-- | Extracting Specifications (Measures + Assumptions) ------------------------
--------------------------------------------------------------------------------
 
{-
allDepNames = concatMap (map declNameString . ms_textual_imps)

declNameString = moduleNameString . unLoc . ideclName . unLoc

patErrorName    = "PatErr"
realSpecName    = "Real"
notRealSpecName = "NotReal"

getSpecs rflag tflag target paths names exts
  = do fs'     <- sortNub <$> moduleImports exts paths names 
       patSpec <- getPatSpec paths tflag
       rlSpec  <- getRealSpec paths rflag
       let fs  = patSpec ++ rlSpec ++ fs'
       liftIO  $ whenLoud $ putStrLn ("getSpecs: " ++ show fs)
       transParseSpecs exts paths S.empty mempty (map snd fs)

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

transParseSpecs _ _ _ specs []
  = return specs
transParseSpecs exts paths seenFiles specs newFiles
  = do newSpecs  <- liftIO $ mapM (\f -> addFst3 f <$> parseSpec f) newFiles
       impFiles  <- moduleImports exts paths $ specsImports newSpecs
       let seenFiles' = seenFiles  `S.union` (S.fromList newFiles)
       let specs'     = specs ++ map (third noTerm) newSpecs
       let newFiles'  = [f | (_,f) <- impFiles, not (f `S.member` seenFiles')]
       transParseSpecs exts paths seenFiles' specs' newFiles'
  where
    specsImports ss = nub $ concatMap (map symbolString . Ms.imports . thd3) ss
    noTerm spec = spec { Ms.decr=mempty, Ms.lazy=mempty, Ms.termexprs=mempty }
    third f (a,b,c) = (a,b,f c)
-}

parseSpec :: FilePath -> IO (ModName, Ms.BareSpec)
parseSpec file
  = do whenLoud $ putStrLn $ "parseSpec: " ++ file
       either Ex.throw return . specParser file =<< readFile file

specParser file str
  | isExtFile Spec file  = specSpecificationP file str
  | isExtFile Hs file    = hsSpecificationP   file str
  | isExtFile LHs file   = lhsSpecificationP  file str
  | otherwise            = exitWithPanic $ "SpecParser: Cannot Parse File " ++ file

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


------------------------------------------------------------------------------
-------------------------------- A CoreBind Visitor --------------------------
------------------------------------------------------------------------------

-- TODO: syb-shrinkage

class CBVisitable a where
  freeVars :: S.HashSet Var -> a -> [Var]
  readVars :: a -> [Var] 
  letVars  :: a -> [Var] 
  literals :: a -> [Literal]

instance CBVisitable [CoreBind] where
  freeVars env cbs = (sortNub xs) \\ ys 
    where xs = concatMap (freeVars env) cbs 
          ys = concatMap bindings cbs
  
  readVars = concatMap readVars
  letVars  = concatMap letVars 
  literals = concatMap literals

instance CBVisitable CoreBind where
  freeVars env (NonRec x e) = freeVars (extendEnv env [x]) e 
  freeVars env (Rec xes)    = concatMap (freeVars env') es 
                              where (xs,es) = unzip xes 
                                    env'    = extendEnv env xs 

  readVars (NonRec _ e)     = readVars e
  readVars (Rec xes)        = concat [x `delete` nubReadVars e |(x, e) <- xes]
    where nubReadVars = sortNub . readVars

  letVars (NonRec x e)      = x : letVars e
  letVars (Rec xes)         = xs ++ concatMap letVars es
    where 
      (xs, es)              = unzip xes

  literals (NonRec _ e)      = literals e
  literals (Rec xes)         = concatMap literals $ map snd xes

instance CBVisitable (Expr Var) where
  freeVars = exprFreeVars
  readVars = exprReadVars
  letVars  = exprLetVars
  literals = exprLiterals

exprFreeVars = go 
  where 
    go env (Var x)         = if x `S.member` env then [] else [x]  
    go env (App e a)       = (go env e) ++ (go env a)
    go env (Lam x e)       = go (extendEnv env [x]) e
    go env (Let b e)       = (freeVars env b) ++ (go (extendEnv env (bindings b)) e)
    go env (Tick _ e)      = go env e
    go env (Cast e _)      = go env e
    go env (Case e x _ cs) = (go env e) ++ (concatMap (freeVars (extendEnv env [x])) cs) 
    go _   _               = []

exprReadVars = go
  where
    go (Var x)             = [x]
    go (App e a)           = concatMap go [e, a] 
    go (Lam _ e)           = go e
    go (Let b e)           = readVars b ++ go e 
    go (Tick _ e)          = go e
    go (Cast e _)          = go e
    go (Case e _ _ cs)     = (go e) ++ (concatMap readVars cs) 
    go _                   = []

exprLetVars = go
  where
    go (Var _)             = []
    go (App e a)           = concatMap go [e, a] 
    go (Lam x e)           = x : go e
    go (Let b e)           = letVars b ++ go e 
    go (Tick _ e)          = go e
    go (Cast e _)          = go e
    go (Case e x _ cs)     = x : go e ++ concatMap letVars cs
    go _                   = []

exprLiterals = go
  where
    go (Lit l)             = [l]
    go (App e a)           = concatMap go [e, a] 
    go (Let b e)           = literals b ++ go e 
    go (Lam _ e)           = go e
    go (Tick _ e)          = go e
    go (Cast e _)          = go e
    go (Case e _ _ cs)     = (go e) ++ (concatMap literals cs) 
    go _                   = []


instance CBVisitable (Alt Var) where
  freeVars env (a, xs, e) = freeVars env a ++ freeVars (extendEnv env xs) e
  readVars (_,_, e)       = readVars e
  letVars  (_,xs,e)       = xs ++ letVars e
  literals (c,_, e)       = literals c ++ literals e


instance CBVisitable AltCon where
  freeVars _ (DataAlt dc) = dataConImplicitIds dc
  freeVars _ _            = []
  readVars _              = []
  letVars  _              = []
  literals (LitAlt l)     = [l]
  literals _              = []



extendEnv = foldl' (flip S.insert)

bindings (NonRec x _) 
  = [x]
bindings (Rec  xes  ) 
  = map fst xes

--------------------------------------------------------------------
------ Strictness --------------------------------------------------
--------------------------------------------------------------------

instance NFData Var
instance NFData SrcSpan

instance PPrint GhcSpec where
  pprint spec =  (text "******* Target Variables ********************")
              $$ (pprint $ tgtVars spec)
              $$ (text "******* Type Signatures *********************")
              $$ (pprintLongList $ tySigs spec)
              $$ (text "******* Assumed Type Signatures *************")
              $$ (pprintLongList $ asmSigs spec)
              $$ (text "******* DataCon Specifications (Measure) ****")
              $$ (pprintLongList $ ctors spec)
              $$ (text "******* Measure Specifications **************")
              $$ (pprintLongList $ meas spec)

instance PPrint GhcInfo where 
  pprint info =   (text "*************** Imported Variables **********")
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

-- | Throw a panic exception
exitWithPanic  :: String -> a 
exitWithPanic  = Ex.throw . errOther . text 

-- | Convert a GHC error into one of ours
instance Result SourceError where 
  result = (`Crash` "Invalid Source") 
         . concatMap errMsgErrors 
         . bagToList 
         . srcErrorMessages
     
errMsgErrors e = [ ErrGhc (errMsgSpan e) (pprint e)] 



{-
getGhcInfo :: Config -> LogicMap -> FilePath -> ModSummary -> SpecM GhcInfo
getGhcInfo cfg logicMap sourcePath summary
  = do let mod   = ms_mod summary
           name  = moduleName mod
           name' = ModName Target name

       liftIO $ cleanFiles sourcePath
       lift   $ load $ LoadUpTo name

       (graph, resolveVertex, lookupVertex) <- ask
       let vertex = fromJust $ lookupVertex name
           ii     = fst3 $ resolveVertex vertex

       let (_, bareSpec) = ii_hspec  ii
           hquals        = ii_hquals ii

       cfg' <- liftIO $ withPragmas cfg sourcePath (Ms.pragmas bareSpec)

       let importNodes = reverse $ map resolveVertex $ filter (/= vertex) $ concatMap T.flatten $ G.dfs graph [vertex]
           importInfo  = map fst3 importNodes
           imports     = map snd3 importNodes
           impSpecs    = map noTerm $ concatMap (\ii -> [ii_hspec ii, ii_ispec ii]) importInfo
       liftIO $ putStrLn $ "imports - " ++ showPpr imports

       phantomImports <- liftIO $ getPhantomImports cfg'
       let allSpecs = (name', bareSpec) : phantomImports ++ impSpecs

       liftIO $ putStrLn $ "spec bundle - " ++ show (sortNub (catMaybes (map ii_path importInfo)))

       let context = (IIModule name) : map (IIDecl . simpleImportDecl) imports
       lift $ setContext context
       liftIO $ putStrLn $ "context - " ++ showPpr context

       modguts   <- lift   $ getGhcModGuts1 summary
       hscEnv    <- lift   $ getSession
       exports   <- lift   $ getExports mod
       coreBinds <- liftIO $ anormalize (not $ nocaseexpand cfg') hscEnv modguts

       let impVs = importVars  coreBinds
           defVs = definedVars coreBinds
           useVs = readVars    coreBinds
           letVs = letVars     coreBinds
           derVs = derivedVars coreBinds $ mgi_is_dfun modguts

       spec <- liftIO $ makeGhcSpec cfg' name' coreBinds (impVs ++ defVs) letVs exports hscEnv logicMap allSpecs

       liftIO $ putStrLn $ "tySigs - "  ++ (show $ map (showPpr.fst) $ tySigs  spec)
       liftIO $ putStrLn $ "asmSigs - " ++ (show $ map (showPpr.fst) $ asmSigs spec)

       liftIO $ putStrLn $ "# tySigs - "  ++ (show $ length $ tySigs     spec)
       liftIO $ putStrLn $ "# asmSigs - " ++ (show $ length $ asmSigs    spec)
       liftIO $ putStrLn $ "# ctors - "   ++ (show $ length $ ctors      spec)
       liftIO $ putStrLn $ "# meas - "    ++ (show $ length $ meas       spec)
       liftIO $ putStrLn $ "# invs - "    ++ (show $ length $ invariants spec)
       liftIO $ putStrLn $ "# ials - "    ++ (show $ length $ ialiases   spec)
       liftIO $ putStrLn $ "# dcop - "    ++ (show $ length $ dconsP     spec)
       liftIO $ putStrLn $ "# tcop - "    ++ (show $ length $ tconsP     spec)
       liftIO $ putStrLn $ "# free - "    ++ (show $ length $ freeSyms   spec)
       liftIO $ putStrLn $ "# qual - "    ++ (show $ length $ qualifiers spec)
       liftIO $ putStrLn $ "# tgtv - "    ++ (show $ length $ tgtVars    spec)
       liftIO $ putStrLn $ "# decr - "    ++ (show $ length $ decr       spec)
       liftIO $ putStrLn $ "# texp - "    ++ (show $ length $ texprs     spec)
       liftIO $ putStrLn $ "# lvrs - "    ++ (show $ S.size $ lvars      spec)
       liftIO $ putStrLn $ "# lazy - "    ++ (show $ S.size $ lazy       spec)
       liftIO $ putStrLn $ "# meaa - "    ++ (show $ length $ measures   spec)

       liftIO $ putStrLn $ "lvars - " ++ (show $ lvars spec)

       liftIO $ putStrLn $ "# impVs - "     ++ (show $ length impVs)
       liftIO $ putStrLn $ "# defVs - "     ++ (show $ length defVs)
       liftIO $ putStrLn $ "# useVs - "     ++ (show $ length useVs)
       liftIO $ putStrLn $ "# letVs - "     ++ (show $ length letVs)
       liftIO $ putStrLn $ "# derVs - "     ++ (show $ length derVs)
       liftIO $ putStrLn $ "# coreBinds - " ++ (show $ length coreBinds)

       let totalHquals = S.toList $ S.unions (hquals : map ii_hquals importInfo)
       liftIO $ putStrLn $ "hquals - " ++ show totalHquals

       return $ GI hscEnv coreBinds derVs impVs letVs useVs totalHquals spec
  where
    noTerm (m, spec) = (m, spec { Ms.decr=mempty, Ms.lazy=mempty, Ms.termexprs=mempty })
-}

