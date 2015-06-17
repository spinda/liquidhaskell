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

import Language.Haskell.Liquid.Spec.Extract
import Language.Haskell.Liquid.Spec.WiredIns


makeGhcSpec :: GhcMonad m => Config -> NameSet -> TypecheckedModule -> m GhcSpec
makeGhcSpec cfg exports mod = runWiredM $ makeGhcSpec' cfg exports mod

makeGhcSpec' :: GhcMonad m => Config -> NameSet -> TypecheckedModule -> WiredM m GhcSpec
makeGhcSpec' cfg exports mod = do
  topSigs <- extractTopSigs mod
  return $ (emptySpec cfg) { tySigs   = map (second dummyLoc) topSigs
                           , exports  = exports
                           , tcEmbeds = M.singleton intTyCon intFTyCon
                           }


emptySpec :: Config -> GhcSpec
emptySpec cfg =
  SP [] [] [] [] [] [] [] [] [] mempty [] [] [] [] mempty mempty mempty cfg mempty [] mempty mempty


