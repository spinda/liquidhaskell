{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Bare.Resolve (
    resolveQualifier

  , resolveRReft

  , resolvePred
  , resolveExpr
  ) where

import Control.Applicative
import Control.Monad.State
import Data.Char
import Text.Parsec.Pos (SourcePos)

import qualified Data.List           as L
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

import Language.Fixpoint.Misc (errorstar)
import Language.Fixpoint.Names (prims)
import Language.Fixpoint.Types (Expr(..), Pred(..), Qualifier(..), Refa(..), Reft(..), Sort(..), Symbol, fTyconSymbol, mkSubst, symbol, symbolFTycon, symbolText, subst)

import Language.Haskell.Liquid.Misc (safeZipWithError, secondM, third3M)
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Bare.Env
import Language.Haskell.Liquid.Bare.Lookup

--------------------------------------------------------------------------------

resolveQualifier :: Qualifier -> BareM Qualifier
resolveQualifier (Q n ps b l)
  = Q n <$> mapM (secondM (resolveSort l)) ps <*> resolvePred l [] b <*> pure l

--------------------------------------------------------------------------------

resolveRReft :: SourcePos -> [Symbol] -> RReft -> BareM RReft
resolveRReft l vargs (U r p s)
  = U <$> resolveReft l vargs r <*> resolvePredicate l vargs p <*> pure s

resolveReft :: SourcePos -> [Symbol] -> Reft -> BareM Reft
resolveReft l vargs (Reft (v, ras))
  = Reft . (v,) <$> mapM (resolveRefa l vargs) ras

resolveRefa :: SourcePos -> [Symbol] -> Refa -> BareM Refa
resolveRefa l vargs (RConc p)
  = RConc <$> resolvePred l vargs p
resolveRefa _ _ r@(RKvar _ _)
  = return r


resolvePredicate :: SourcePos -> [Symbol] -> Predicate -> BareM Predicate
resolvePredicate l vargs (Pr pvs)
  = Pr <$> mapM (resolveUsedPVar l vargs) pvs

resolveUsedPVar :: SourcePos -> [Symbol] -> UsedPVar -> BareM UsedPVar
resolveUsedPVar l vargs (PV n t v as)
  = PV n t v <$> mapM (third3M (resolveExpr l vargs)) as

--------------------------------------------------------------------------------

resolvePred :: SourcePos -> [Symbol] -> Pred -> BareM Pred
resolvePred l vargs
  = go
  where
    go (PBexp e@(EApp (Loc l' f) es))
      = do aliases <- gets (predAliases.rtEnv)
           case M.lookup f aliases of
             Just alias ->
               expandEApp l' alias <$> mapM (resolveExpr l vargs) es
             Nothing ->
               PBexp <$> resolveExpr l vargs e
    go (PBexp e)
      = PBexp <$> resolveExpr l vargs e

    go (PAtom r e1 e2)
      = PAtom r <$> resolveExpr l vargs e1 <*> resolveExpr l vargs e2

    go (PAnd ps)
      = PAnd <$> mapM go ps
    go (POr ps)
      = POr <$> mapM go ps
    go (PNot p)
      = PNot <$> go p

    go (PImp p q)
      = PImp <$> go p <*> go q
    go (PIff p q)
      = PIff <$> go p <*> go q

    go PTrue
      = return PTrue
    go PFalse
      = return PFalse
    go PTop
      = return PTop

    go (PAll _ _)
      = errorstar "Bare.Resolve.resolvePred can't handle PAll"


resolveExpr :: SourcePos -> [Symbol] -> Expr -> BareM Expr
resolveExpr l vargs
  = go
  where
    go (EApp lf@(Loc l' f) es)
      = do aliases <- gets (exprAliases.rtEnv)
           case M.lookup f aliases of
             Just alias ->
               expandEApp l' alias <$> mapM go es
             Nothing ->
               EApp <$> go_fsym lf <*> mapM go es
    go (EVar f)
      = (EVar . val) <$> (go_fsym $ Loc l f)

    go (EIte p e1 e2)
      = EIte <$> resolvePred l vargs p <*> go e1 <*> go e2

    go (ECst e s)
      = ECst <$> go e <*> resolveSort l s

    go (EBin op e1 e2)
      = EBin op <$> go e1 <*> go e2

    go e@(ESym _)
      = return e
    go e@(ECon _)
      = return e

    go EBot
      = return EBot

    go (ELit _ _)
      = errorstar "Bare.Resolve.resolveExpr can't handle ELit"

    go_fsym lf@(Loc _ f)
      | f `elem` vargs
        = return lf
      | otherwise
        = resolveFSymbol lf


-- TODO: Turn this into a proper Error instead of dying
expandEApp l alias es
  = subst su $ rtBody alias
  where su  = mkSubst $ safeZipWithError msg (rtVArgs alias) es
        msg = "Malformed alias application at " ++ show l ++ "\n\t"
               ++ show (rtName alias)
               ++ " defined at " ++ show (rtPos alias)
               ++ "\n\texpects " ++ show (length $ rtVArgs alias)
               ++ " arguments but it is given " ++ show (length es)

--------------------------------------------------------------------------------

resolveSort :: SourcePos -> Sort -> BareM Sort
resolveSort l
  = go
  where
    go (FApp tc ss)
      | tcs' `elem` prims
        = FApp tc <$> mapM go ss
      | otherwise
        = FApp <$> (symbolFTycon.Loc l'.symbol <$> lookupGhcTyCon tcs) <*> mapM go ss
      -- FIXME: Nested where...
      where
        tcs@(Loc l' tcs')
          = fTyconSymbol tc

    go (FObj f)
      = (FObj . val) <$> (resolveFSymbol $ Loc l f)

    go (FFunc i ss)
      = FFunc i <$> mapM go ss

    go s@(FVar _)
      = return s

    go FInt
      = return FInt
    go FReal
      = return FReal
    go FNum
      = return FNum

--------------------------------------------------------------------------------

resolveFSymbol :: LocSymbol -> BareM LocSymbol
resolveFSymbol lf@(Loc l f)
  | f `elem` prims
    = return lf
  | isCon f
    = do v <- lookupGhcVar lf
         let qs = symbol v
         addSym (qs, v)
         return $ Loc l qs
  | otherwise
    = return lf

isCon :: Symbol -> Bool
isCon c
  | Just (c,_) <- T.uncons $ symbolText c = isUpper c
  | otherwise                             = False

-- FIXME: This is a hack left in because makeSymbols in Bare/Misc.hs depends
--        on it. There's got to be a better way do what it does; right now
--        it's depending on a side-effect of name resolution, in an
--        abstraction-breaking way.
addSym x = modify $ \be -> be { varEnv = (varEnv be) `L.union` [x] }

