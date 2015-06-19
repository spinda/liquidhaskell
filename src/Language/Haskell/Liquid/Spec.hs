{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Liquid.Spec (
    GhcSpec(..)
  , makeGhcSpec
  ) where

import GHC

import GhcMonad
import NameSet
import TysWiredIn

import Control.Arrow

import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)

import Language.Fixpoint.Types

import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Extract


makeGhcSpec :: Config -> NameSet -> TypecheckedModule -> Ghc GhcSpec
makeGhcSpec cfg exports mod = runSpecM $ makeGhcSpec' cfg exports mod

makeGhcSpec' :: Config -> NameSet -> TypecheckedModule -> SpecM GhcSpec
makeGhcSpec' cfg exports mod = do
  topSigs <- extractTySigs mod
  tySyns  <- extractTySyns mod
  liftIO $ mapM_ printTySyn tySyns
  return $ (emptySpec cfg) { tySigs   = map (second dummyLoc) topSigs
                           , exports  = exports
                           , tcEmbeds = M.singleton intTyCon intFTyCon
                           }
  where
    printTySyn (RTA name targs vargs body _ _) = whenLoud $
      do putStrLn $ "=== type synonym: " ++ showpp name ++ " ==="
         putStrLn $ "targs: " ++ showpp targs
         putStrLn $ "vargs: " ++ showpp vargs
         putStrLn $ "body: " ++ showpp body


emptySpec :: Config -> GhcSpec
emptySpec cfg =
  SP [] [] [] [] [] [] [] [] [] mempty [] [] [] [] mempty mempty mempty cfg mempty [] mempty mempty

