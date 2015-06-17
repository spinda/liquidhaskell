{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec.Extract (
    extractTopSigs
--  , extractLocalSigs
--  , extractTySyns
  ) where

import GHC

import HscTypes
import MonadUtils
import TcRnTypes
import Var

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Reify

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

extractTopSigs :: TypecheckedModule -> SpecM [(Var, SpecType)]
extractTopSigs = mapMaybeM go . typeEnvElts . tcg_type_env . fst . tm_internals_
  where
    go (AnId id) = (Just . (id, )) <$> reifyRTy (idType id)
    go _         = return Nothing

--extractLocalSigs :: TypecheckedModule -> SpecM [(Var, SpecType)]
--extractLocalSigs

