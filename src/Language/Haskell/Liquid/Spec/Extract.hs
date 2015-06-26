{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Spec.Extract (
    extractTySigs
  , extractTySyns
  , extractTcEmbeds
  , extractInlines
  ) where

import GHC

import Bag
import HsBinds
import HsExpr
import HscTypes
import MonadUtils
import Name
import Panic
import SrcLoc
import TcRnTypes
import TyCon
import TypeRep
import Var

import Control.Arrow

import Data.List
import Data.Maybe
import Data.Monoid

import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Types

import Language.Haskell.Liquid.CoreToLogic
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.RType
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.Reify
import Language.Haskell.Liquid.Spec.WiredIns

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

extractTySyns :: TypecheckedModule -> SpecM [RTAlias RTyVar SpecType]
extractTySyns mod =
  mapM go tysyns
  where
    things = modInfoTyThings $ tm_checked_module_info mod
    tycons = mapMaybe (\case { ATyCon tc -> Just tc; _ -> Nothing }) things
    tysyns = mapMaybe (\tc -> (tc, ) <$> synTyConDefn_maybe tc) tycons
    go (tc, (tvs, rhs)) = do
      rhs' <- reifyRTy rhs
      evs  <- ai_exprParams <$> lookupAnnInfo tc
      return $
        RTA { rtName  = symbol tc
            , rtTArgs = map rTyVar tvs
            , rtVArgs = map symbolRTyVar evs
            , rtBody  = rhs'
            -- TODO: Extract type synonym position data
            , rtPos   = dummyPos "TypeAlias"
            , rtPosE  = dummyPos "TypeAlias"
            }

--------------------------------------------------------------------------------

extractTcEmbeds :: SpecM (TCEmb TyCon)
extractTcEmbeds = M.fromList <$> (mapMaybeM (go . second ai_ftcEmbed) =<< getAllAnnInfo)
  where
    go (_, Nothing) =
      return Nothing
    go (name, Just ftc) = do
      thing <- lookupName name
      return $ case thing of
        Just (ATyCon tc) -> Just (tc, ftc)
        _                -> Nothing

--------------------------------------------------------------------------------

extractInlines :: SpecM [(LocSymbol, TInline)]
extractInlines = do
  vars <- extractInlineVars
  M.fromList <$> mapM ofInline vars
  where
    ofInline var = do
      def <- fromJust <$> lookupCoreBind var
      let span = getSrcSpan var
      let sym  = Loc (srcSpanSourcePos span) (srcSpanSourcePosE span) (symbol $ getName var)
      case runToLogic mempty $ coreToFun sym var def of
        Left (xs, e) -> return (sym, TI (symbol <$> xs) e)
        Right err    -> panic err

extractInlineVars :: SpecM [Var]
extractInlineVars = mapMaybeM (go . second ai_isInline) =<< getAllAnnInfo
  where
    go (_, False) =
      return Nothing
    go (name, True) = do
      thing <- lookupName name
      return $ case thing of
        Just (AnId id) -> Just id
        _              -> Nothing

