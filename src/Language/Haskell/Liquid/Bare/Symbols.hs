module Language.Haskell.Liquid.Bare.Symbols (
    makeSymbols
  ) where

import Id
import Type
import Var

import Control.Arrow
import Control.Applicative
import Data.Maybe (isNothing)

import Language.Fixpoint.Misc (sortDiff, sortNub)
import Language.Fixpoint.Types

import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types


makeSymbols vs xs' xts yts ivs
  = (syms, su)
  where
    syms  = filter ((`elem` xs) . fst) $ map (symbol &&& id) vs
    su    = mkSubst $ map (second mkVarExpr) syms

    xs    = sortNub $ zs ++ zs' ++ zs''
    zs    = concatMap freeSymbols (snd <$> xts) `sortDiff` xs'
    zs'   = concatMap freeSymbols (snd <$> yts) `sortDiff` xs'
    zs''  = concatMap freeSymbols ivs           `sortDiff` xs'

freeSymbols ty = sortNub $ concat $ efoldReft (\_ _ -> []) (\ _ -> ()) f (\_ -> id) emptySEnv [] (val ty)
  where 
    f γ _ r xs = let Reft (v, _) = toReft r in 
                 [ x | x <- syms r, x /= v, not (x `memberSEnv` γ)] : xs


mkVarExpr v 
  | isFunVar v = EApp (varFunSymbol v) []
  | otherwise  = EVar (symbol v)

varFunSymbol = dummyLoc . dataConSymbol . idDataCon 

isFunVar v   = isDataConWorkId v && not (null αs) && isNothing tf
  where
    (αs, t)  = splitForAllTys $ varType v 
    tf       = splitFunTy_maybe t

