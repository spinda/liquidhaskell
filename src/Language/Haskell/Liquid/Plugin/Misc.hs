module Language.Haskell.Liquid.Plugin.Misc (
    -- * Utilities in TcPluginM
    tcPluginDynFlags
  , tcPluginFindModule
  , tcPluginGhc
  , tcPluginHomeModules
  , tcPluginIsHsBootOrSig
  , tcPluginModSummary
  , tcPluginModule
  , tcPluginModuleGraph
  ) where

import GHC

import FastString
import GhcMonad
import HscTypes
import Module
import TcPluginM
import TcRnTypes

import Data.IORef

--------------------------------------------------------------------------------
-- Utilities in TcPluginM ------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the current DynFlags
tcPluginDynFlags :: TcPluginM DynFlags
tcPluginDynFlags = hsc_dflags <$> getTopEnv

-- | Find a module based on a name and optional package qualifier, as if in an
-- import statement
tcPluginFindModule :: ModuleName -> Maybe FastString -> TcPluginM Module
tcPluginFindModule mod pkg = tcPluginGhc $ findModule mod pkg

-- | Embed the Ghc monad in TcPluginM
tcPluginGhc :: Ghc a -> TcPluginM a
tcPluginGhc act = do
  env <- getTopEnv
  ref <- tcPluginIO $ newIORef env
  tcPluginIO $ unGhc act $ Session ref

-- | Is the current module a .hs-boot or .hsig?
tcPluginIsHsBootOrSig :: TcPluginM Bool
tcPluginIsHsBootOrSig = (isHsBootOrSig . tcg_src . fst) <$> getEnvs

-- | Get a set of all home modules
tcPluginHomeModules :: TcPluginM ModuleSet
tcPluginHomeModules = (mkModuleSet . map ms_mod) <$> tcPluginModuleGraph

-- | Look up the ModSummary for a home module
tcPluginModSummary :: ModuleName -> TcPluginM ModSummary
tcPluginModSummary = tcPluginGhc . getModSummary

-- | Get the current module being checked
tcPluginModule :: TcPluginM Module
tcPluginModule = (tcg_mod . fst) <$> getEnvs

-- | Get the module dependency graph
tcPluginModuleGraph :: TcPluginM ModuleGraph
tcPluginModuleGraph = hsc_mod_graph <$> getTopEnv

