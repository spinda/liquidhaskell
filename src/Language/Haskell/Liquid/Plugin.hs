{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.Liquid.Plugin (
    plugin
  ) where

import GHC

import ErrUtils
import GhcMonad
import GhcPlugins
import HscTypes
import Module
import TcPluginM
import TcRnTypes

import Data.IORef

import System.IO.Unsafe

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Iface
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Plugin.Config
import Language.Haskell.Liquid.Plugin.Driver
import Language.Haskell.Liquid.Plugin.Misc

-- | GHC plugin entry point
plugin :: Plugin
plugin = printCopyright $ defaultPlugin { tcPlugin = pluginInstall }

-- | Print the LiquidHaskell copyright notice when first loaded
printCopyright :: a -> a
printCopyright = seq $ unsafePerformIO $ putStrLn $
  "LiquidHaskell Copyright 2009-15 Regents of the University of California. All Rights Reserved."
{-# NOINLINE printCopyright #-}

-- | Install LiquidHaskell typechecker plugin
pluginInstall :: [CommandLineOption] -> Maybe TcPlugin
pluginInstall opts = Just $ TcPlugin
  { tcPluginInit  = pluginInit opts
  , tcPluginSolve = \_ _ _ _ -> return $ TcPluginOk [] []
  , tcPluginStop  = pluginMain
  }

-- | LiquidHaskell plugin initialization
pluginInit :: [CommandLineOption] -> TcPluginM Config
pluginInit = tcPluginIO . parsePluginOpts

-- | LiquidHaskell plugin main pipeline
pluginMain :: Config -> TcPluginM ()
pluginMain cfg = unlessInteractive $ whenNoErrs $ do
  mod     <- tcPluginModule
  summary <- tcPluginModSummary $ moduleName mod
  processModule cfg globalIfaceCache summary

-- | Global .lqhi cache reference
globalIfaceCache :: IfaceCache
globalIfaceCache = unsafePerformIO newIfaceCache
{-# NOINLINE globalIfaceCache #-}

-- | Skip execution if the target module is a (fake) interactive module
unlessInteractive :: TcPluginM () -> TcPluginM ()
unlessInteractive act = do
  mod <- tcPluginModule
  if isInteractiveModule mod
     then return ()
     else act

-- | Skip execution if the module already failed to typecheck
whenNoErrs :: TcPluginM () -> TcPluginM ()
whenNoErrs act = do
  errsRef <- (tcl_errs . snd) <$> getEnvs
  errs    <- tcPluginIO $ readIORef errsRef
  dflags  <- tcPluginDynFlags
  if errorsFound dflags errs
     then return ()
     else act

