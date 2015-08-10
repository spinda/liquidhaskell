{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Spec.Check (
    -- * Validate Specifications
    checkGhcSpec

    -- * Duplication Checking
  , checkDupLogic
  , checkDupEmbeds

    -- * Local Checks
  , checkLocalEmbeds
  ) where

import GHC hiding (Located)

import CoreSyn
import Id
import Name
import TysWiredIn
import Var

import Control.Arrow ((***), (&&&))

import Data.Function
import Data.Maybe

import qualified Data.HashMap.Strict as M

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Misc
import Language.Fixpoint.Sort (checkSortedReftFull)
import Language.Fixpoint.Types

import Language.Haskell.Liquid.Errors ()
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Literals
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.PredType
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.Visitors
import Language.Haskell.Liquid.WiredIn

--------------------------------------------------------------------------------
-- Validate Specifications -----------------------------------------------------
--------------------------------------------------------------------------------

checkGhcSpec :: [CoreBind] -> GhcSpec -> GhcSpec -> [Error]
checkGhcSpec cbs fullSpec@SP{tcEmbeds=emb} SP{..} =
     concatMap (checkSig emb env) (M.toList tySigs)
  ++ concatMap (checkSig emb env) (M.toList asmSigs)
  ++ concatMap (checkSig emb env) (M.toList ctors)
  ++ concatMap (checkInv emb env) invariants
  ++ concatMap (checkIAl emb env) ialiases
  where
    env = buildSEnv cbs fullSpec

buildSEnv :: [CoreBind] -> GhcSpec -> SEnv SortedReft
buildSEnv cbs sp = fromListSEnv binds
  where
    emb         = tcEmbeds sp
    binds       =  [(symbol v, rSort $ sort m) | (v, m) <- M.toList $ meas sp]
                ++ [(symbol v, rSort t) | (v, Loc _ _ t) <- M.toList $ ctors sp]
                ++ [(x,        vSort v) | (x, v) <- M.toList $ freeSyms sp, isConLikeId v]
                ++ [(val x   , rSort stringrSort) | Just (ELit x s) <- mkLit <$> lconsts, isString s]
    rSort       :: (PPrint r, Reftable r) => RRType r -> SortedReft
    rSort       = rTypeSortedReft emb
    vSort       = rSort . varRSort
    varRSort    :: Var -> RSort
    varRSort    = ofType . varType
    lconsts     = literals cbs
    stringrSort :: RSort
    stringrSort = ofType stringTy
    isString s  = rTypeSort emb stringrSort == s


checkSig :: TCEmb TyCon -> SEnv SortedReft -> (Var, Located SpecType) -> [Error]
checkSig emb env (var, ty) = checkRType mkErr emb env (val ty)
  where
    mkErr = ErrTySpec (locatedSrcSpan ty) (pprint var) (val ty)

checkInv :: TCEmb TyCon -> SEnv SortedReft -> Located SpecType -> [Error]
checkInv emb env ty = checkRType mkErr emb env (val ty)
  where
    mkErr = ErrInvt (locatedSrcSpan ty) (val ty)

checkIAl :: TCEmb TyCon -> SEnv SortedReft -> (Located SpecType, Located SpecType) -> [Error]
checkIAl emb env (t1, t2) = go t1 ++ go t2
  where
    go    ty = checkRType (mkErr ty) emb env (val ty)
    mkErr ty = ErrIAl (locatedSrcSpan ty) (val ty)


checkRType :: (PPrint r, Reftable r) => (Doc -> Error) -> TCEmb TyCon -> SEnv SortedReft -> RRType (UReft r) -> [Error]
checkRType mkErr emb env ty = reverse $ efoldReft cb (rTypeSortedReft emb) f insertPEnv env [] ty
  where
    cb c ts         = classBinds (rRCls c ts)
    f env me r errs = maybeToList (checkReft mkErr emb env me r) ++ errs
    insertPEnv p γ  = insertsSEnv γ (mapSnd (rTypeSortedReft emb) <$> pbinds p)
    pbinds p        = (pname p, pvarRType p :: RSort)
                    : [(x, t) | (t, x, _) <- pargs p]

checkReft :: (PPrint r, Reftable r)
          => (Doc -> Error)
          -> TCEmb TyCon
          -> SEnv SortedReft
          -> Maybe (RRType (UReft r))
          -> UReft r -> Maybe Error
checkReft _ _ _ Nothing _ =
  Nothing -- TODO:RPropP/Ref case, not sure how to check these yet.
checkReft mkErr emb env (Just ty) _ =
  (mkErr . (dr $+$)) <$> checkSortedReftFull env' r
  where
    r    = rTypeSortedReft emb ty
    dr   = text "Sort Error in Refinement:" <+> pprint r
    env' = foldl (\e (x, s) -> insertSEnv x (RR s mempty) e) env wiredSortedSyms

--------------------------------------------------------------------------------
-- Duplication Checking --------------------------------------------------------
--------------------------------------------------------------------------------

checkDupLogic :: [Located Var] -> [Located Var] -> [Error]
checkDupLogic inlines meas = map mkErr ds
  where
    ds = dupsBy ((==) `on` snd) (compare `on` snd) ls
    ls = map ("inline", ) inlines ++ map ("measure", ) meas

    mkErr xs@((_, lv):_) =
      ErrDupLogic (locatedSrcSpan lv) (pprint $ val lv) $
        map (text *** locatedSrcSpan) xs
    mkErr [] = error "checkDupLogic.mkError: empty list"

checkDupEmbeds :: [(TyCon, Located FTycon)] -> [Error]
checkDupEmbeds embeds = map mkErr ds
  where
    ds = dupsBy ((==) `on` fst) (compare `on` snd) embeds

    mkErr xs@((c, lf):_) =
      ErrDupEmbeds (locatedSrcSpan lf) (pprint c) $
        map (((pprint . val) &&& locatedSrcSpan) . snd) xs
    mkErr [] = error "checkDupEmbeds.mkError: empty list"

--------------------------------------------------------------------------------
-- Local Checks ----------------------------------------------------------------
--------------------------------------------------------------------------------

checkLocalEmbeds :: Module -> Maybe Module -> [(TyCon, Located FTycon)] -> [Error]
checkLocalEmbeds mod sig = map mkErr . filter (not . isLocal . fst)
  where
    isLocal (nameModule . getName -> tcMod) = 
      tcMod == mod || case sig of { Nothing -> False; Just sigMod -> tcMod == sigMod }

    mkErr :: (TyCon, Located FTycon) -> Error
    mkErr (tc, lf) = ErrEmbedScope (locatedSrcSpan lf) (pprint tc)

