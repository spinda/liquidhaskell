{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec.Reify (
    -- * Reify TH Names in RType
    reifyRTy
  ) where

import GHC hiding (Located)

import BasicTypes
import TyCon
import TysPrim
import TysWiredIn
import Var

import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import qualified Data.HashSet as S

import qualified Language.Haskell.TH.Syntax as TH

import Language.Fixpoint.Names (prims)
import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.TH.Types
import Language.Haskell.Liquid.TH.WiredIns

import Language.Haskell.Liquid.Spec.Lookup

--------------------------------------------------------------------------------
-- Reify TH Names in RType -----------------------------------------------------
--------------------------------------------------------------------------------

reifyRTy :: Monoid r => ARType r -> Ghc (RRType r)
reifyRTy (RVar tv r) =
  return $ RVar (stringRTyVar tv) r
reifyRTy (RFun b i o r) =
  RFun b <$> reifyRTy i <*> reifyRTy o <*> pure r
reifyRTy (RAllT tv ty) =
  RAllT (stringRTyVar tv) <$> reifyRTy ty
reifyRTy (RAllP pv ty) =
  RAllP <$> reifyPVar pv <*> reifyRTy ty
reifyRTy (RAllS sb ty) =
  RAllS sb <$> reifyRTy ty
reifyRTy (RApp c as ps r)
  | val c == cxtArrowTcName, [cxt, ty] <- as = do
    cxt' <- reifyRTy cxt
    ty'  <- reifyRTy ty
    return $ foldr (rFun dummySymbol) ty' $ setTupleSort ConstraintTuple <$> splitTupleRTy cxt'
  | otherwise = do
    rApp <$> reifyTyCon c (length as) <*> mapM reifyRTy as <*> mapM reifyRef ps <*> pure r
reifyRTy (RAllE b a t) =
  RAllE b <$> reifyRTy a <*> reifyRTy t
reifyRTy (REx b e t) =
  REx b <$> reifyRTy e <*> reifyRTy t
reifyRTy (RExprArg e) =
  return $ RExprArg e
reifyRTy (RAppTy t1 t2 r) =
  RAppTy <$> reifyRTy t1 <*> reifyRTy t2 <*> pure r
reifyRTy (RRTy env ref obl ty) =
  RRTy <$> mapM (secondM reifyRTy) env <*> pure ref <*> pure obl <*> reifyRTy ty
reifyRTy (RHole r) =
  return $ RHole r

reifyTyCon :: Located TH.Name -> Int -> Ghc TyCon
reifyTyCon c arity
  | val c == funArrowTcName = return funTyCon
  | val c == tupleTcName    = return $ tupleTyCon BoxedTuple arity
  | val c == listTcName     = return listTyCon
  | val c == equalityTcName = return eqTyCon
  | otherwise               = lookupGhcTyCon c

reifyPVar :: APVar -> Ghc RPVar
reifyPVar (PV name kind arg args) =
  PV name <$> traverse reifyRTy kind <*> pure arg <*> mapM (first3M reifyRTy) args

reifyRef :: Monoid r => RTProp (Located TH.Name) String r -> Ghc (RTProp RTyCon RTyVar r)
reifyRef (RPropP as r) = (`RPropP` r) <$> mapM (secondM reifyRTy) as
reifyRef (RProp  as b) = RProp <$> mapM (secondM reifyRTy) as <*> reifyRTy b
reifyRef (RHProp as h) = RHProp <$> mapM (secondM reifyRTy) as <*> traverse reifyRTy h

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

splitTupleRTy :: RRType r -> [RRType r]
splitTupleRTy (RApp c as _ _) | isTupleTyCon $ rtc_tc c = as
splitTupleRTy t = [t]

setTupleSort :: TupleSort -> RRType r -> RRType r
setTupleSort sort = go
  where
    go (RApp c as ps r) =
      let tc = rtc_tc c
          c' | isTupleTyCon tc && tupleTyConSort tc /= sort =
                 c { rtc_tc = tupleTyCon sort (tupleTyConArity tc)}
             | otherwise = c
      in  RApp c' (go <$> as) ps r
    go (RAppTy t1 t2 r) =
      RAppTy (go t1) (go t2) r
    go (RFun b i o r) =
      RFun b (go i) (go o) r
    go (RAllT tv ty) =
      RAllT tv (go ty)
    go (RAllP pv ty) =
      RAllP pv (go ty)
    go (RAllS sb ty) =
      RAllS sb (go ty)
    go (RAllE b a t) =
      RAllE b a (go t)
    go (REx b e t) =
      REx b e (go t)
    go (RRTy env ref obl ty) =
      RRTy env ref obl (go ty)
    go t = t

