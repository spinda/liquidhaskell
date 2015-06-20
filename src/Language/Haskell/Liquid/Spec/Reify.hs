{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec.Reify (
    reifyRTy
  , reifyRReft
  , reifyReft
  , reifyRefa
  , reifyPred
  , reifyExpr

    -- TODO: Eliminate this export
  , isExprParam
  ) where

import GHC hiding (Located)

import ConLike
import DataCon
import DynFlags
import FastString
import IdInfo
import Name
import Panic
import TyCon
import Type
import TypeRep
import Unique
import Var

import qualified Outputable as Out

import Data.Monoid

import Text.Parsec.Pos

import Language.Fixpoint.Names
import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------
-- Reify RType -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRTy :: Type -> SpecM SpecType

reifyRTy (TyVarTy tv)  =
  return $ rVar tv

reifyRTy (AppTy t1 t2) =
  RAppTy <$> reifyRTy t1 <*> reifyRTy t2 <*> pure mempty

reifyRTy (TyConApp tc as) = go =<< getWiredIns
  where
    go wis
      | tc == tc_Bind wis, [s, b, _a] <- as =
        invalidBind =<< reifyLocated reifySymbol s b
      | tc == tc_Refine wis, [a, b, p] <- as =
        strengthen <$> reifyRTy a <*> reifyRReft b p
      | Just (tenv, rhs, as') <- tcExpandTyCon_maybe tc as =
        reifyRTy $ mkAppTys (substTy (mkTopTvSubst tenv) rhs) as'
      | otherwise =
        rApp tc <$> mapM reifyRTy as <*> pure [] <*> pure mempty

reifyRTy (FunTy i o) = do
  (b, i') <- reifyBind i
  RFun b <$> reifyRTy i' <*> reifyRTy o <*> pure mempty

reifyRTy (ForAllTy tv ty) = do
  RAllT (rTyVar tv) <$> reifyRTy ty

reifyRTy ty@(LitTy _)  =
  malformed "type" ty

--------------------------------------------------------------------------------
-- Reify RReft -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRReft :: Type -> Type -> SpecM RReft
reifyRReft b r = do
  r' <- reifyReft b r
  return $ (mempty :: RReft) { ur_reft = r' }


reifyReft :: Type -> Type -> SpecM Reft
reifyReft b r = do
  b' <- reifySymbol b
  r' <- reifyRefa r
  return $ Reft (b', r')


reifyRefa :: Type -> SpecM Refa
reifyRefa = fmap Refa . reifyPred

--------------------------------------------------------------------------------
-- Reify Pred -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyPred :: Type -> SpecM Pred
reifyPred ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc as)
      | tc == pc_PTrue wis, [] <- as =
        return PTrue
      | tc == pc_PFalse wis, [] <- as =
        return PFalse
      | tc == pc_PAnd wis, [x, y] <- as =
        PAnd <$> mapM reifyPred [x, y]
      | tc == pc_POr wis, [x, y] <- as =
        POr <$> mapM reifyPred [x, y]
      | tc == pc_PNot wis, [x] <- as =
        PNot <$> reifyPred x
      | tc == pc_PImp wis, [x, y] <- as =
        PImp <$> reifyPred x <*> reifyPred y
      | tc == pc_PIff wis, [x, y] <- as =
        PIff <$> reifyPred x <*> reifyPred y
      | tc == pc_PExp wis, [x] <- as =
        PBexp <$> reifyExpr x
      | tc == pc_PAtom wis, [brel, e1, e2] <- as =
        PAtom <$> reifyBrel brel <*> reifyExpr e1 <*> reifyExpr e2
      | tc == pc_PTop wis, [] <- as =
        return PTop
    go _ _ = malformed "predicate" ty

--------------------------------------------------------------------------------
-- Reify Expr ------------------------------------------------------------------
--------------------------------------------------------------------------------

reifyExpr :: Type -> SpecM Expr
reifyExpr ty = (`go` ty) =<< getWiredIns
  where
    -- Special case for type synonym extraction: expression params aren't
    -- substituted yet
    go wis (TyVarTy tv)
      | isExprParam wis tv, ('ℯ':var) <- getOccString tv =
        return $ EVar $ symbol var
    go wis (TyConApp tc as)
      | tc == pc_ECon wis, [c] <- as =
        ECon <$> reifyConstant c
      | tc == pc_EBdr wis, [s] <- as =
        EVar <$> reifySymbol s
      | tc == pc_ECtr wis, [_, s, t] <- as =
        (EVar . val) <$> reifyLocated reifyDataCon s t
      | tc == pc_ENeg wis, [e] <- as =
        ENeg <$> reifyExpr e
      | tc == pc_EBin wis, [bop, e1, e2] <- as =
        EBin <$> reifyBop bop <*> reifyExpr e1 <*> reifyExpr e2
      | tc == pc_EIte wis, [pred, e1, e2] <- as =
        EIte <$> reifyPred pred <*> reifyExpr e1 <*> reifyExpr e2
      | tc == pc_EBot wis, [] <- as =
        return EBot
    go _ _ = malformed "expression" ty

isExprParam :: WiredIns -> TyVar -> Bool
isExprParam wis tv = tyVarKind tv == exprKind
  where
    exprKind = TyConApp (tc_Expr wis) []



reifyConstant :: Type -> SpecM Constant
reifyConstant ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [a])
      | tc == pc_I wis =
        I <$> reifyNat a
    go _ _ = malformed "constant" ty

reifyBrel :: Type -> SpecM Brel
reifyBrel ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [])
      | tc == pc_Eq  wis = return Eq
      | tc == pc_Ne  wis = return Ne
      | tc == pc_Gt  wis = return Gt
      | tc == pc_Ge  wis = return Ge
      | tc == pc_Lt  wis = return Lt
      | tc == pc_Le  wis = return Le
      | tc == pc_Ueq wis = return Ueq
      | tc == pc_Une wis = return Une
    go _ _ = malformed "binary relation" ty

reifyBop :: Type -> SpecM Bop
reifyBop ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [])
      | tc == pc_Plus  wis = return Plus
      | tc == pc_Minus wis = return Minus
      | tc == pc_Times wis = return Times
      | tc == pc_Div   wis = return Div
      | tc == pc_Mod   wis = return Mod
    go _ _ = malformed "binary operator" ty

--------------------------------------------------------------------------------
-- Reify Data Constructor ------------------------------------------------------
--------------------------------------------------------------------------------

reifyDataCon :: Type -> SpecM Symbol
reifyDataCon (TyConApp tc _)
  | Just dc <- isPromotedDataCon_maybe tc =
    return $ varSymbol $ dataConWorkId dc
reifyDataCon ty =
  malformed "data constructor" ty

--------------------------------------------------------------------------------
-- Reify Components ------------------------------------------------------------
--------------------------------------------------------------------------------

reifyLocated :: (Type -> SpecM a) -> Type -> Type -> SpecM (Located a)
reifyLocated f s x = (`go` s) =<< getWiredIns
  where
    go wis (TyConApp tc [filename, startLine, startCol, endLine, endCol])
      | tc == pc_Span wis = do
        filename'  <- reifyString filename
        startLine' <- fromIntegral <$> reifyNat startLine
        startCol'  <- fromIntegral <$> reifyNat startCol
        endLine'   <- fromIntegral <$> reifyNat endLine
        endCol'    <- fromIntegral <$> reifyNat endCol
        x'         <- f x
        return $ Loc (newPos filename' startLine' startCol')
                     (newPos filename' endLine'   endCol'  )
                     x'
      | otherwise = malformed "loocation annotation" s
    go _ _ = malformed "location annotation" s


reifyBind :: Type -> SpecM (Symbol, Type)
reifyBind ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [_s, b, a])
      | tc == tc_Bind wis = (, a) <$> reifySymbol b
    go _ _ = ((, ty) . tempSymbol "db") <$> mkFreshInt


reifyString :: Type -> SpecM String
reifyString (LitTy (StrTyLit s)) = return $ unpackFS s
reifyString ty                   = malformed "symbol" ty

reifySymbol :: Type -> SpecM Symbol
reifySymbol = fmap symbol . reifyString

reifyNat :: Type -> SpecM Integer
reifyNat (LitTy (NumTyLit n)) = return n
reifyNat ty                   = malformed "natural number" ty

--------------------------------------------------------------------------------
-- Error Messages --------------------------------------------------------------
--------------------------------------------------------------------------------

malformed :: String -> Type -> m a
malformed desc ty = panic $
  "Malformed LiquidHaskell " ++ desc ++ " encoding: " ++ showPpr ty

invalidBind :: Located Symbol -> m a
invalidBind lb = panic $
  "Bind cannot appear at this location: " ++ show lb

