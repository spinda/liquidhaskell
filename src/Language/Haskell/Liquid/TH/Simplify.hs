module Language.Haskell.Liquid.TH.Simplify (
    -- * Simplify RType to TH Type
    simplifyRTy
  ) where

import Control.Arrow

import Data.List

import Language.Haskell.TH.Syntax

import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.TH.Misc
import Language.Haskell.Liquid.TH.Types
import Language.Haskell.Liquid.TH.WiredIns

--------------------------------------------------------------------------------
-- Simplify RType to TH Type ---------------------------------------------------
--------------------------------------------------------------------------------

simplifyRTy :: AnnoType -> Type
simplifyRTy (RVar v _) =
  VarT $ mkName v
simplifyRTy (RFun _ i o _) =
  ArrowT `AppT` simplifyRTy i `AppT` simplifyRTy o
simplifyRTy t@(RAllT _ _)
  = let (tvs, cxt, t') = splitSigmaRTy t
    in  ForallT (PlainTV . mkName <$> tvs)
                (simplifyRTy <$> cxt)
                (simplifyRTy t')
simplifyRTy (RAllP _ t) =
  simplifyRTy t
simplifyRTy (RAllS _ t) =
  simplifyRTy t
simplifyRTy t@(RApp c as _ _)
  | (cxt@(_:_), t') <- splitPhiRTy t =
    ForallT [] (simplifyRTy <$> cxt) (simplifyRTy t')
  | otherwise =
    let c' | val c == funArrowTcName = ArrowT
           | val c == tupleTcName    = TupleT (length as)
           | val c == listTcName     = ListT
           | otherwise               = ConT $ val c
    in  foldl' AppT c' (simplifyRTy <$> filter (not . isExprArg) as)
simplifyRTy (RAllE _ _ t) =
  simplifyRTy t
simplifyRTy (REx  _ _ t) =
  simplifyRTy t
simplifyRTy (RExprArg _) =
  error "simplifyRTy: RExprArg"
simplifyRTy (RAppTy t1 t2 _) =
  simplifyRTy t1 `AppT` simplifyRTy t2
simplifyRTy (RRTy _ _ _ t) =
  simplifyRTy t
simplifyRTy (RHole _) =
  error "simplifyRTy: RHole"

