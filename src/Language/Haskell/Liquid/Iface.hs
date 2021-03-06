{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Iface (
    -- * Iface Cache
    IfaceCache
  , newIfaceCache

    -- * Iface Types
  , IfaceData(..)
  , mkIfaceFingerprint

    -- * Load Iface Data
  , tryLoadTargetIface
  , getHomeIface
  , getPkgIface

    -- * Save Iface Data
  , putTargetIface
  ) where

import GHC

import DynFlags
import FastString
import Fingerprint
import HscTypes
import Module
import MonadUtils
import PackageConfig
import Packages
import PrelNames
import TcPluginM
import UniqFM

import Data.IORef
import Data.Maybe
import Data.Version

import System.Directory

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Iface.File
import Language.Haskell.Liquid.Iface.Types

--------------------------------------------------------------------------------
-- Load Iface Data -------------------------------------------------------------
--------------------------------------------------------------------------------

tryLoadTargetIface :: IfaceCache -> ModLocation -> Fingerprint -> TcPluginM (FilePath, Bool)
tryLoadTargetIface cache loc fingerprint = do
  exists <- tcPluginIO $ doesFileExist path
  (path, ) <$> if exists
     then do ifaceData@ID{..} <- readIfaceData path
             if ifaceFingerprint == fingerprint
                then True <$ tcPluginIO (extendIfaceCache cache ifaceData)
                else return False
     else return False
  where
    path = mkIfacePath loc

getHomeIface :: IfaceCache -> Module -> TcPluginM (IfaceData GhcSpec)
getHomeIface cache mod = do
  cached <- tcPluginIO $ lookupIfaceCache cache mod
  case cached of
    Just ifaceData -> return ifaceData
    Nothing -> do
      found <- findIfaceForMod mod
      ifaceData <- case found of
        -- TODO: Any way we can handle this better?
        --       Really, GHC should force a recompile if
        --       using different plugins...
        Nothing -> error $
          "Missing .lqhi file for home module dependency "
            ++ showPpr mod
            ++ ". Clean and recompile, or re-run GHC with -fforce-recomp."
        Just path -> readIfaceData path
      ifaceData <$ tcPluginIO (extendIfaceCache cache ifaceData)

getPkgIface :: IfaceCache -> Config -> Module -> TcPluginM (IfaceData GhcSpec)
getPkgIface cache cfg mod = do
  mod'   <- rewriteWiredMod cfg mod
  cached <- tcPluginIO $ lookupIfaceCache cache mod'
  case cached of
    Just ifaceData ->
      return ifaceData
    Nothing -> do
      found     <- findIfaceForMod mod'
      ifaceData <- case found of
        Nothing   -> return $ emptyIfaceData mod'
        Just path -> readIfaceData path
      ifaceData <$ tcPluginIO (extendIfaceCache cache ifaceData)

--------------------------------------------------------------------------------
-- Save Iface Data -------------------------------------------------------------
--------------------------------------------------------------------------------

putTargetIface :: IfaceCache -> Maybe FilePath -> IfaceData GhcSpec -> IO ()
putTargetIface cache path ifaceData = do
  maybe (return ()) (`writeIfaceData` ifaceData) path
  extendIfaceCache cache ifaceData

--------------------------------------------------------------------------------
-- Rewrite Modules with Wired-In Specifications --------------------------------
--------------------------------------------------------------------------------

rewriteWiredMod :: Config -> Module -> TcPluginM Module
rewriteWiredMod cfg mod
  | not (noWiredInSpecs cfg) && hasWiredInSpecs pkg = findWiredModule mod
  | otherwise                                       = return mod
  where
    pkg = modulePackageKey mod

hasWiredInSpecs :: PackageKey -> Bool
hasWiredInSpecs = flip elem
  [ primPackageKey
  , basePackageKey
  , integerPackageKey
  ]

findWiredModule :: Module -> TcPluginM Module
findWiredModule mod = do
  result <- findImportedModule wiredModName (Just wiredPkgName)
  return $ case result of
    Found _ mod' -> mod'
    _            -> mod
  where
    wiredModName = mkModuleName $ "Liquid." ++ moduleNameString (moduleName mod)
    wiredPkgName = fsLit "liquidhaskell-std"

--------------------------------------------------------------------------------
-- IfaceCache Type Internals ---------------------------------------------------
--------------------------------------------------------------------------------

newtype IfaceCache = IC (IORef (ModuleEnv (IfaceData GhcSpec)))

newIfaceCache :: IO IfaceCache
newIfaceCache = IC <$> newIORef emptyModuleEnv

lookupIfaceCache :: IfaceCache -> Module -> IO (Maybe (IfaceData GhcSpec))
lookupIfaceCache (IC ref) mod = (`lookupModuleEnv` mod) <$> readIORef ref

extendIfaceCache :: IfaceCache -> IfaceData GhcSpec -> IO ()
extendIfaceCache (IC ref) ifaceData = atomicModifyIORef ref $ \env ->
  (extendModuleEnv env (ifaceModule ifaceData) ifaceData, ())

