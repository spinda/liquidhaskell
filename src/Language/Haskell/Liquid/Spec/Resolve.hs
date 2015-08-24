{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Haskell.Liquid.Spec.Resolve (
    -- * Resolve Names in Refinements
    resolveGhcSpec
  ) where

import GHC hiding (Located)

import DataCon
import Id
import Name
import SrcLoc hiding (Located)
import TyCon
import Type
import Var

import Control.Arrow
import Control.Monad.State

import Data.Char
import Data.Either
import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import qualified Data.Text           as T

import Language.Fixpoint.Misc
import Language.Fixpoint.Names (dropModuleNames, prims)
import Language.Fixpoint.Types hiding (Predicate)

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Lookup

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

resolveGhcSpec :: GhcSpec -> Ghc GhcSpec
resolveGhcSpec spec = do
  (spec', freeSyms') <- runResolveM $ resolve spec
  return $ spec' { freeSyms = freeSyms' }

--------------------------------------------------------------------------------
-- The ResolveM Monad ----------------------------------------------------------
--------------------------------------------------------------------------------

type ResolveM = StateT ResolveState Ghc

data ResolveState = RS { rs_location  :: !SrcSpan
                       , rs_fixScope  :: !(S.HashSet Symbol)
                       , rs_freeSyms  :: !(M.HashMap Symbol Var)
                       }


runResolveM :: ResolveM a -> Ghc (a, M.HashMap Symbol Var)
runResolveM act = second rs_freeSyms <$> runStateT act initState
  where 
    initState = RS noSrcSpan (S.fromList prims) mempty 


getLocation :: ResolveM SrcSpan
getLocation = gets rs_location

isInFixScope :: Symbol -> ResolveM Bool
isInFixScope s = gets (S.member s . rs_fixScope)

withLocation :: SrcSpan -> ResolveM a -> ResolveM a
withLocation l act = do
  l' <- gets rs_location
  modify $ \rs -> rs { rs_location = l }
  res <- act
  modify $ \rs -> rs { rs_location = l' }
  return res

withFixScope :: [Symbol] -> ResolveM a -> ResolveM a
withFixScope scope act = do
  scope' <- gets rs_fixScope
  modify $ \rs -> rs { rs_fixScope = foldr S.insert scope' scope }
  res <- act
  modify $ \rs -> rs { rs_fixScope = scope' }
  return res

visitFreeSym :: Symbol -> Var -> ResolveM ()
visitFreeSym sym var = modify $ \rs ->
  rs { rs_freeSyms = M.insert sym var $ rs_freeSyms rs }

--------------------------------------------------------------------------------
-- Inner Resolve Traversal -----------------------------------------------------
--------------------------------------------------------------------------------

class Resolve r where
  resolve :: r -> ResolveM r


instance Resolve () where
  resolve = return

instance Resolve r => Resolve [r] where
  resolve = mapM resolve

instance Resolve r => Resolve (Located r) where
  resolve x = withLocation (locatedSrcSpan x) (traverse resolve x)


instance Resolve GhcSpec where
  resolve spec = do
    tySigs'   <- traverse resolve $ tySigs   spec
    asmSigs'  <- traverse resolve $ asmSigs  spec
    ctors'    <- traverse resolve $ ctors    spec
    rtEnv'    <- traverse resolve $ rtEnv    spec
    tinlines' <- traverse resolve $ tinlines spec
    meas'     <- traverse resolve $ meas     spec
    return $
      spec { tySigs   = tySigs'
           , asmSigs  = asmSigs'
           , ctors    = ctors'
           , rtEnv    = rtEnv'
           , tinlines = tinlines'
           , meas     = meas'
           }

instance Resolve r => Resolve (RTAlias tv r) where
  resolve rta@(RTA _ es _) = withFixScope es $ traverse resolve rta

instance Resolve TInline where
  resolve (TI args body) = withFixScope args $
    TI args <$> either (fmap Left . resolve) (fmap Right . resolve) body

instance Resolve r => Resolve (Measure r id) where
  resolve (M sort defs) = M sort <$> mapM (secondM resolve) defs

-- TODO: Does this follow the correct scoping rules?
instance Resolve r => Resolve (RRType r) where
  resolve (RAllT tv ty) =
    RAllT tv <$> resolve ty
  resolve (RAllP pv ty) = withFixScope [pname pv] $
    RAllP <$> resolve pv <*> resolve ty
  resolve (RAllS sb ty) = withFixScope [sb] $
    RAllS sb <$> resolve ty
  resolve (RAllE b a ty) = withFixScope [b] $
    RAllE b <$> resolve a <*> resolve ty
  resolve (REx b e ty) = withFixScope [b] $
    REx b <$> resolve e <*> resolve ty
  resolve (RRTy env ref obl ty) = withFixScope (fst <$> env) $
    RRTy <$> mapM (secondM resolve) env <*> resolve ref <*> pure obl <*> resolve ty
  resolve t@RFun{} = do
    let (binds, args, refts, res) = bkArrow t
    withFixScope binds $ do
      args'  <- resolve args
      refts' <- resolve refts
      res'   <- resolve res
      return $ mkArrow [] [] [] (zip3 binds args' refts') res'
  resolve (RVar tv r) =
    RVar tv <$> resolve r
  resolve (RApp c as ps r) =
    RApp c <$> resolve as <*> resolve ps <*> resolve r
  resolve (RExprArg e) =
    RExprArg <$> resolve e
  resolve (RAppTy t1 t2 r) =
    RAppTy <$> resolve t1 <*> resolve t2 <*> resolve r
  resolve (RHole r) =
    RHole <$> resolve r

-- TODO: Is the scoping correct here?
instance (Resolve τ, Resolve r, Resolve t) => Resolve (Ref τ r t) where
  resolve (RPropP as r) = withFixScope (map fst as) $
    RPropP <$> mapM (secondM resolve) as <*> resolve r
  resolve (RProp as b) = withFixScope (map fst as) $
    RProp  <$> mapM (secondM resolve) as <*> resolve b
  resolve (RHProp as h) = withFixScope (map fst as) $
    RHProp <$> mapM (secondM resolve) as <*> resolve h

instance Resolve r => Resolve (World r) where
  resolve (World hs) = World <$> resolve hs

instance Resolve r => Resolve (HSeg r) where
  resolve (HBind addr val) = withFixScope [addr] $ HBind addr <$> resolve val
  resolve (HVar pv)        = HVar <$> resolve pv


instance Resolve r => Resolve (UReft r) where
  resolve (U r p s) = U <$> resolve r <*> resolve p <*> pure s

instance Resolve Predicate where
  resolve (Pr pvs) = Pr <$> resolve pvs

-- TODO: Is the scoping correct here?
instance Resolve r => Resolve (PVar r) where
  resolve (PV n t v as) = withFixScope (n : v : map snd3 as) $
    PV n t v <$> mapM (third3M resolve) as

instance Resolve r => Resolve (PVKind r) where
  resolve = traverse resolve

instance Resolve Reft where
  resolve (Reft (s, r)) = withFixScope [s] $
    Reft . (s, ) <$> resolve r

instance Resolve Refa where
  resolve (Refa p) = Refa <$> resolve p


instance Resolve Pred where
  resolve (PAnd  ps     ) = PAnd    <$> resolve ps
  resolve (POr   ps     ) = POr     <$> resolve ps
  resolve (PNot  p      ) = PNot    <$> resolve p
  resolve (PImp  p1 p2  ) = PImp    <$> resolve p1 <*> resolve p2
  resolve (PIff  p1 p2  ) = PIff    <$> resolve p1 <*> resolve p2
  resolve (PBexp e      ) = PBexp   <$> resolve e
  resolve (PAtom b e1 e2) = PAtom b <$> resolve e1 <*> resolve e2
  resolve (PAll  s p    ) = withFixScope (fst <$> s) $
    PAll <$> mapM (secondM resolve) s <*> resolve p
  resolve p = return p

instance Resolve Expr where
  resolve (EVar v      ) = resolveEVar v
  resolve (EApp f es   ) = resolveEApp f =<< resolve es
  resolve (ENeg e      ) = ENeg   <$> resolve e
  resolve (EBin b e1 e2) = EBin b <$> resolve e1 <*> resolve e2
  resolve (EIte p e1 e2) = EIte   <$> resolve p  <*> resolve e1 <*> resolve e2
  resolve (ECst e s    ) = ECst   <$> resolve e  <*> resolve s
  resolve e = return e

instance Resolve Sort where
  resolve (FFunc i ss) = FFunc i <$> resolve ss
  resolve (FApp  c ss) = FApp    <$> resolveTyCon c <*> resolve ss
  resolve s = return s


resolveEVar :: Symbol -> ResolveM Expr
resolveEVar v = do
  loc       <- getLocation
  (v', var) <- resolveVar v
  return $ case var of
    Just var' | isFunVar var' ->
      EApp (srcSpanLocated loc $ dataConSymbol $ idDataCon var') []
    _  -> EVar v'

resolveEApp :: LocSymbol -> [Expr] -> ResolveM Expr
resolveEApp f es = do
  (f', _) <- withLocation (locatedSrcSpan f) $ resolveVar (val f)
  return $ EApp (const f' <$> f) es


resolveVar :: Symbol -> ResolveM (Symbol, Maybe Var)
resolveVar s = go_scope
  where
    go_scope = do
      bind <- isInFixScope s
      if bind
         then return (s, Nothing)
         else go_lookup
    go_lookup | isCon s   = go_con
              | otherwise = go_var
    go_con = do
      loc <- getLocation
      var <- lift $ lookupGhcVar $ srcSpanLocated loc s
      let s' = varSymbol var
      visitFreeSym s' var
      return (s', Just var)
    go_var = do
      var <- lift $ tryLookup lookupGhcVar s
      return (maybe s varSymbol var, var)

resolveTyCon :: FTycon -> ResolveM FTycon
resolveTyCon tc
  | tcs' `elem` prims = return tc
  | otherwise         = symbolFTycon . Loc l l' . symbol <$> lift (lookupGhcTyCon tcs)
  where
    tcs@(Loc l l' tcs') = fTyconSymbol tc

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

isCon :: Symbol -> Bool
isCon c
  | Just (c,_) <- T.uncons $ symbolText $ dropModuleNames c = isUpper c
  | otherwise                                               = False

isFunVar :: Var -> Bool
isFunVar v = isDataConId v && not (null as) && isNothing tf
  where
    (as, t) = splitForAllTys $ varType v 
    tf      = splitFunTy_maybe t

