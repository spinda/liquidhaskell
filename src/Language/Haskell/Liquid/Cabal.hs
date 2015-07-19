module Language.Haskell.Liquid.Cabal (
    -- * Setup.hs Hooks for Cabal
    liquidMain
  , liquidHooks
  , liquidPostCopy
  , liquidPostInst

    -- * LiquidHaskell Setup Operations
  , installLqhiFiles
  ) where

import Control.Monad

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Verbosity

--------------------------------------------------------------------------------
-- Setup.hs Hooks for Cabal ----------------------------------------------------
--------------------------------------------------------------------------------

-- | Simple @main@ for a Cabal setup script with LiquidHaskell integration
--
-- > liquidMain = defaultMainWithHooks liquidHooks
liquidMain :: IO ()
liquidMain = defaultMainWithHooks liquidHooks

-- | Hooks for a Cabal setup script with LiquidHaskell integration
--
-- > liquidHooks = simpleUserHooks { postCopy = liquidPostCopy
-- >                               , postInst = liquidPostInst
-- >                               }
liquidHooks :: UserHooks
liquidHooks = simpleUserHooks { postCopy = liquidPostCopy
                              , postInst = liquidPostInst
                              }

-- | Call 'installLqiuFiles' during @cabal copy@
liquidPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
liquidPostCopy _args flags =
  installLqhiFiles (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags)

-- | Call 'installLqiuFiles' during @cabal install@
liquidPostInst ::  Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
liquidPostInst _args flags =
  installLqhiFiles (fromFlag $ installVerbosity flags) NoCopyDest

--------------------------------------------------------------------------------
-- LiquidHaskell Setup Operations ----------------------------------------------
--------------------------------------------------------------------------------

-- | Copy a library's generated .lqhi files to the same directories as its .hi
-- files
installLqhiFiles :: Verbosity -> CopyDest -> PackageDescription -> LocalBuildInfo -> IO ()
installLqhiFiles verbosity dest pkg lbi = withLib pkg $ \lib -> when vanilla $ do
  lqhiFiles <- findModuleFiles [buildDir lbi] ["lqhi"] $ libModules lib
  installOrdinaryFiles verbosity targetDir lqhiFiles
  where
    vanilla   = withVanillaLib lbi
    targetDir = libdir $ absoluteInstallDirs pkg lbi dest

