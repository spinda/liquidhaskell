{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Liquid.Spec (
    GhcSpec(..)
  , makeGhcSpec
  ) where

import GHC

import GhcMonad
import HscTypes
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
import Language.Haskell.Liquid.Spec.Reify


makeGhcSpec :: Config -> NameSet -> TypecheckedModule -> ModGuts -> Ghc GhcSpec
makeGhcSpec cfg exports mod guts = runSpecM (makeGhcSpec' cfg exports mod) guts

makeGhcSpec' :: Config -> NameSet -> TypecheckedModule -> SpecM GhcSpec
makeGhcSpec' cfg exports mod = do
  liftIO $ whenLoud $ putStrLn "extraction started..."
  topSigs  <- extractTySigs mod
  tySyns   <- extractTySyns mod
  tcEmbeds <- extractTcEmbeds
  inlines  <- extractInlines
  liftIO $ mapM_ printTySyn tySyns
  liftIO $ mapM_ printInline inlines
  return $ (emptySpec cfg) { tySigs   = map (second dummyLoc) topSigs
                           , exports  = exports
                           , tcEmbeds = tcEmbeds
                           }
  where
    printTySyn (RTA name targs vargs body _ _) = whenLoud $
      do putStrLn $ "=== type synonym: " ++ showpp name ++ " ==="
         putStrLn $ "targs: " ++ showpp targs
         putStrLn $ "vargs: " ++ showpp vargs
         putStrLn $ "body: " ++ showpp body
    printInline id = whenLoud $ putStrLn $ "=== inline: " ++ showpp id


emptySpec :: Config -> GhcSpec
emptySpec cfg =
  SP [] [] [] [] [] [] [] [] [] mempty [] [] [] [] mempty mempty mempty cfg mempty [] mempty mempty

