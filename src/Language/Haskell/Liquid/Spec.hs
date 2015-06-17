module Language.Haskell.Liquid.Spec (
    GhcSpec(..)
  , makeGhcSpec
  ) where

import GHC

import NameSet
import TysWiredIn

import Control.Arrow

import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Extract


makeGhcSpec :: Config -> NameSet -> TypecheckedModule -> Ghc GhcSpec
makeGhcSpec cfg exports mod = runSpecM $ makeGhcSpec' cfg exports mod

makeGhcSpec' :: Config -> NameSet -> TypecheckedModule -> SpecM GhcSpec
makeGhcSpec' cfg exports mod = do
  topSigs <- extractTySigs mod
  return $ (emptySpec cfg) { tySigs   = map (second dummyLoc) topSigs
                           , exports  = exports
                           , tcEmbeds = M.singleton intTyCon intFTyCon
                           }


emptySpec :: Config -> GhcSpec
emptySpec cfg =
  SP [] [] [] [] [] [] [] [] [] mempty [] [] [] [] mempty mempty mempty cfg mempty [] mempty mempty


