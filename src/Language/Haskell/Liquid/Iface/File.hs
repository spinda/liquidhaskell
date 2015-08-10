{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.Liquid.Iface.File (
    -- * Located Liquid Interface Files
    mkIfacePath
  , findIfaceForMod

    -- * Read/Write Liquid Interface Files
  , writeIfaceData
  , readIfaceData
  ) where

import GHC

import Finder
import HscTypes
import MonadUtils
import PrelNames
import TcPluginM hiding (getEnvs)
import TcRnMonad hiding (getTopEnv)

import Data.IORef

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Iface.Convert
import Language.Haskell.Liquid.Iface.Ghc
import Language.Haskell.Liquid.Iface.Types

import System.Directory
import System.FilePath

--------------------------------------------------------------------------------
-- Locate Liquid Interface Files -----------------------------------------------
--------------------------------------------------------------------------------

mkIfacePath :: ModLocation -> FilePath
mkIfacePath = (`replaceExtension` "lqhi") . ml_hi_file

findIfaceForMod :: Module -> TcPluginM (Maybe FilePath)
findIfaceForMod mod
  | mod == gHC_PRIM = return Nothing
  | otherwise = do
    hscEnv <- getTopEnv
    result <- tcPluginIO $ findExactModule hscEnv mod
    case result of
      Found loc _ -> do
        let path = mkIfacePath loc
        exists <- tcPluginIO $ doesFileExist path
        return $ if exists then Just path else Nothing
      _           -> return Nothing
   
--------------------------------------------------------------------------------
-- Read/Write Liquid Interface Files --------------------------------------
--------------------------------------------------------------------------------

writeIfaceData :: FilePath -> IfaceData GhcSpec -> IO ()
writeIfaceData path ifaceData = do
  createDirectoryIfMissing True $ takeDirectory path
  writeIfaceFile path $ toIface ifaceData

readIfaceData :: FilePath -> TcPluginM (IfaceData GhcSpec)
readIfaceData path = unsafeTcPluginTcM $ do
  ifaceData@ID{..} <- readIfaceFile path
  tcRnIfL ifaceModule $ fromIface ifaceData

