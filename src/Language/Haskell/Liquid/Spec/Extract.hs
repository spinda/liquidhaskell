{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Spec.Extract (
    extractTySigs
  , extractTySyns
  ) where

import GHC

import Annotations
import Bag
import HsBinds
import HsExpr
import HscTypes
import MonadUtils
import NameEnv
import Serialized
import SrcLoc
import TcRnTypes
import TyCon
import Var

import Data.Maybe

import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.RType
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Reify

--------------------------------------------------------------------------------

extractTySigs :: TypecheckedModule -> SpecM [(Var, SpecType)]
extractTySigs mod = do
  liftIO $ putStrLn $ showPpr $ tm_typechecked_source mod
  liftIO $ putStrLn $ showPpr ids
  mapM (\id -> (id, ) <$> reifyRTy (idType id)) ids
  where
    ids = idsFromSource $ tm_typechecked_source mod

--------------------------------------------------------------------------------

idsFromSource :: TypecheckedSource -> [Id]
idsFromSource = idsFromBinds

idsFromBinds :: LHsBinds Id -> [Id]
idsFromBinds = concatMap idsFromBind . bagToList

-- TODO: Re-check that this always extracts the right Ids
idsFromBind :: LHsBindLR Id Id -> [Id]
idsFromBind (unLoc -> bind@(AbsBinds {})) =
  map abe_poly (abs_exports bind) ++ idsFromBinds (abs_binds bind)
idsFromBind (unLoc -> bind@(FunBind {})) =
  idsFromMatches $ fun_matches bind
idsFromBind _ = []

idsFromMatches :: MatchGroup Id (LHsExpr Id) -> [Id]
idsFromMatches = concatMap idsFromMatch . mg_alts

idsFromMatch :: LMatch Id (LHsExpr Id) -> [Id]
idsFromMatch = idsFromGRHSs . m_grhss . unLoc

idsFromGRHSs :: GRHSs Id (LHsExpr Id) -> [Id]
idsFromGRHSs = idsFromLocalBinds . grhssLocalBinds

idsFromLocalBinds :: HsLocalBinds Id -> [Id]
idsFromLocalBinds (HsValBinds binds) = idsFromValBinds binds
idsFromLocalBinds _                  = []

-- TODO: How does recursion affect this?
idsFromValBinds :: HsValBinds Id -> [Id]
idsFromValBinds (ValBindsIn  binds _) = idsFromBinds binds
idsFromValBinds (ValBindsOut binds _) = concatMap (idsFromBinds . snd) binds

--------------------------------------------------------------------------------

extractTySyns :: TypecheckedModule -> ModGuts -> SpecM [RTAlias RTyVar SpecType]
extractTySyns mod guts =
  mapM go tysyns
  where
    annenv = mkAnnEnv $ mg_anns guts
    things = modInfoTyThings $ tm_checked_module_info mod
    tycons = mapMaybe (\case { ATyCon tc -> Just tc; _ -> Nothing }) things
    tysyns = mapMaybe (\tc -> (tc, ) <$> synTyConDefn_maybe tc) tycons
    go (tc, (tvs, rhs)) = do
      rhs' <- reifyRTy rhs
      return $
        RTA { rtName  = symbol tc
            , rtTArgs = map rTyVar tvs
            , rtVArgs = map symbolRTyVar $ lookupVArgs annenv tc
            , rtBody  = rhs'
            -- TODO: Extract type synonym position data
            , rtPos   = dummyPos "TypeAlias"
            , rtPosE  = dummyPos "TypeAlias"
            }

lookupVArgs :: AnnEnv -> TyCon -> [Symbol]
lookupVArgs env = concatMap go . findAnns deserializeWithData env . NamedTarget . getName
  where
    go (ExprParams vargs) = map symbol vargs

