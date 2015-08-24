{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec.Expand (
    expandGhcSpec
  ) where

import GHC hiding (Located)

import Digraph
import DynFlags
import MonadUtils
import Name
import TyCon
import Var

import Control.Arrow
import Control.Monad
import Control.Monad.State

import Data.Either
import Data.Foldable
import Data.Hashable
import Data.List
import Data.Maybe

import qualified Data.HashMap.Strict as M

import qualified Language.Haskell.TH.Syntax as TH

import Language.Fixpoint.Misc
import Language.Fixpoint.Types

import Language.Haskell.Liquid.Errors ()
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Check

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

expandGhcSpec :: GhcSpec -> GhcSpec -> Ghc GhcSpec
expandGhcSpec scope spec =
  runExpandM (expandGhcSpec' spec) (tinlines scope) (rtEnv scope)

expandGhcSpec' :: GhcSpec -> ExpandM GhcSpec
expandGhcSpec' spec@SP{..} = do
  tinlines' <- expandInlines  tinlines
  meas'     <- expandMeasures meas
  rtEnv'    <- expandTySyns   rtEnv
  tySigs'   <- expandTySigs   tySigs
  asmSigs'  <- expandTySigs   asmSigs
  ctors'    <- expandTySigs   ctors
  return $ spec { tinlines = tinlines'
                , meas     = meas'
                , rtEnv    = rtEnv'
                , tySigs   = tySigs'
                , asmSigs  = asmSigs'
                , ctors    = ctors'
                }

--------------------------------------------------------------------------------
-- The ExpandM Monad -----------------------------------------------------------
--------------------------------------------------------------------------------

type ExpandM = StateT ExpandState Ghc

data ExpandState = ES { es_tinlines :: !(M.HashMap Symbol TInline)
                      , es_rtEnv    :: !RTEnv
                      }

runExpandM :: ExpandM a -> M.HashMap Var TInline -> RTEnv -> Ghc a
runExpandM act tinlines rtEnv = evalStateT act initState
  where
    initState = ES tinlines' rtEnv
    tinlines' = M.fromList $ map (first varSymbol) $ M.toList tinlines


lookupInline :: Symbol -> ExpandM (Maybe TInline)
lookupInline s = gets (M.lookup s . es_tinlines)

lookupTySyn :: TyCon -> ExpandM (Maybe (RTAlias RTyVar SpecType))
lookupTySyn s = gets (M.lookup s . es_rtEnv)


putInline :: Var -> TInline -> ExpandM ()
putInline var ti = modify $ \es ->
  es { es_tinlines = M.insert (varSymbol var) ti $ es_tinlines es }

putTySyn :: TyCon -> RTAlias RTyVar SpecType -> ExpandM ()
putTySyn tc rta = modify $ \es ->
  es { es_rtEnv = M.insert tc rta $ es_rtEnv es }

--------------------------------------------------------------------------------
-- Expand Inlines --------------------------------------------------------------
--------------------------------------------------------------------------------

expandInlines :: M.HashMap Var TInline -> ExpandM (M.HashMap Var TInline)
expandInlines =
  graphExpand varSymbol inlineEdges inlineCycle expandInline

inlineEdges :: TInline -> [Symbol]
inlineEdges (TI _ b) = either predEdges exprEdges b

inlineCycle :: [Var] -> Error
inlineCycle []       = error "inlineCycle: empty list"
inlineCycle vs@(v:_) = 
  ErrInlineCycle (getSrcSpan v) ((getSrcSpan &&& pprint) <$> vs)

expandInline :: Var -> TInline -> ExpandM TInline
expandInline var (TI as b) = do
  ti <- TI as <$> either (fmap Left . expandPred span) (fmap Right . expandExpr span) b
  ti <$ putInline var ti
  where
    span = getSrcSpan var

--------------------------------------------------------------------------------
-- Expand Measures -------------------------------------------------------------
--------------------------------------------------------------------------------

expandMeasures :: M.HashMap LocSymbol SpecMeasure -> ExpandM (M.HashMap LocSymbol SpecMeasure)
expandMeasures = M.traverseWithKey expandMeasure

expandMeasure :: LocSymbol -> SpecMeasure -> ExpandM SpecMeasure
expandMeasure name (M sort defs) =
  M sort <$> mapM (secondM $ expandMeasureBody $ locatedSrcSpan name) defs

expandMeasureBody :: SrcSpan -> SpecType -> ExpandM SpecType
expandMeasureBody l b = do
  res <- mapReftM (expandRReft l) $ ty_res rep
  return $ fromRTypeRep $ rep { ty_res = res }
  where
    rep = toRTypeRep b

--------------------------------------------------------------------------------
-- Expand Type Synonyms --------------------------------------------------------
--------------------------------------------------------------------------------

expandTySyns :: RTEnv -> ExpandM RTEnv
expandTySyns =
  graphExpand id tySynEdges tySynCycle expandTySyn

tySynEdges :: RTAlias RTyVar SpecType -> [TyCon]
tySynEdges = nub . go_rty . val . rtBody
  where
    go_rty :: RRType r -> [TyCon]
    go_rty (RVar _ _)       = []
    go_rty (RFun _ i o _)   = go_rty i ++ go_rty o
    go_rty (RAllT _ t)      = go_rty t
    go_rty (RAllP pv t)     = go_pv pv ++ go_rty t
    go_rty (RAllS _ t)      = go_rty t
    go_rty (RApp c as ps _) = rtc_tc c : (concatMap go_rty as ++ concatMap go_ref ps)
    go_rty (RAllE _ arg t)  = go_rty arg ++ go_rty t
    go_rty (REx _ ex t)     = go_rty ex ++ go_rty t
    go_rty (RExprArg _)     = []
    go_rty (RAppTy t1 t2 _) = go_rty t1 ++ go_rty t2
    go_rty (RRTy env _ _ t) = concatMap (go_rty . snd) env ++ go_rty t
    go_rty (RHole _)        = []

    go_pv (PV _name kind _arg args) =
      foldMap go_rty kind ++ concatMap (go_rty . fst3) args

    go_ref (RPropP as _) = concatMap (go_rty . snd) as
    go_ref (RProp  as b) = concatMap (go_rty . snd) as ++ go_rty b
    go_ref (RHProp as h) = concatMap (go_rty . snd) as ++ foldMap go_rty h

tySynCycle :: [TyCon] -> Error
tySynCycle []       = error "tySynCycle: empty list"
tySynCycle cs@(c:_) =
  ErrSynCycle (getSrcSpan c) ((getSrcSpan &&& pprint) <$> cs)

expandTySyn :: TyCon -> RTAlias RTyVar SpecType -> ExpandM (RTAlias RTyVar SpecType)
expandTySyn tc (RTA ts es b) = do
  rta <- RTA ts es <$> expandSpecType b
  rta <$ putTySyn tc rta

--------------------------------------------------------------------------------
-- Expand Type Signatures ------------------------------------------------------
--------------------------------------------------------------------------------

expandTySigs :: M.HashMap Var (Located SpecType) -> ExpandM (M.HashMap Var (Located SpecType))
expandTySigs = traverse expandSpecType

--------------------------------------------------------------------------------
-- Expansion Traversals Over RRType --------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Does mapReftM cover everything we need?
expandSpecType :: Located SpecType -> ExpandM (Located SpecType)
expandSpecType t = traverse (go $ locatedSrcSpan t) t
  where
    go l = mapReftM (expandRReft l) <=< (expandRRType l)


expandRRType :: (PPrint r, Reftable r, UReftable r) => SrcSpan -> RRType r -> ExpandM (RRType r)
expandRRType _ (RVar tv r) =
  return $ RVar tv r
expandRRType l (RFun b i o r) =
  RFun b <$> expandRRType l i <*> expandRRType l o <*> pure r
expandRRType l (RAllT tv ty) =
  RAllT tv <$> expandRRType l ty
expandRRType l (RAllP pv ty) =
  RAllP <$> expandRPVar l pv <*> expandRRType l ty
expandRRType l (RAllS sb ty) =
  RAllS sb <$> expandRRType l ty
expandRRType l (RApp c as ps r) =
  expandRApp l c as ps r
expandRRType l (RAllE b a ty) =
  RAllE b <$> expandRRType l a <*> expandRRType l ty
expandRRType l (REx b e ty) =
  REx  b <$> expandRRType l e <*> expandRRType l ty
expandRRType _ (RExprArg e) =
  return $ RExprArg e
expandRRType l (RAppTy t1 t2 r) =
  RAppTy <$> expandRRType l t1 <*> expandRRType l t2 <*> pure r
expandRRType l (RRTy env ref obl ty) =
  RRTy <$> mapM (secondM $ expandRRType l) env
       <*> pure ref <*> pure obl <*> expandRRType l ty
expandRRType _ (RHole r) =
  return $ RHole r


expandRPVar :: SrcSpan -> RPVar -> ExpandM RPVar
expandRPVar l (PV name kind arg args) =
  PV name <$> traverse (expandRRType l) kind
          <*> pure arg
          <*> mapM (first3M $ expandRRType l) args

expandRTProp :: (PPrint r, Reftable r, UReftable r) => SrcSpan -> RTProp RTyCon RTyVar r -> ExpandM (RTProp RTyCon RTyVar r)
expandRTProp l (RPropP as r) =
  (`RPropP` r) <$> mapM (secondM $ expandRRType l) as
expandRTProp l (RProp  as b) =
  RProp <$> mapM (secondM $ expandRRType l) as <*> expandRRType l b
expandRTProp l (RHProp as h) =
  RHProp <$> mapM (secondM $ expandRRType l) as <*> traverse (expandRRType l) h


expandRApp l c as ps r = do
  (ts, es)   <- splitArgs
  (syn, evs) <- tcInfo

  when (length es /= length evs) $ lift $ throwGhc $
    ErrExprArgCount l (pprint $ rtc_tc c) (length es) (length evs) errType

  case syn of
    Nothing -> do
      RApp c <$> mapM (expandRRType l) ts <*> mapM (expandRTProp l) ps <*> pure r
    Just (tvs, rhs) -> do
      -- If LiberalTypeSynonyms is enabled, partial type synonym application is
      -- valid within the arguments of a type synonym application, as long as
      -- everything is fully applied after expansion.
      --
      -- In that case, we need to substitute *first*, then run expansion over
      -- the result of substitution. Otherwise (the normal case) we expand the
      -- arguments first, for performance.
      lts <- (Opt_LiberalTypeSynonyms `xopt`) <$> lift getSessionDynFlags
      ts' <- if lts then return ts else mapM (expandRRType l) ts
      let tsu  = zipWith (\a t -> (a, toRSort t, t)) tvs ts'
      let esu  = mkSubst $ zip evs $ map val es
      let ty'  = subsTyVars_meet tsu $ subst esu rhs
      let ty'' = ty' `strengthen` r
      if lts then expandRRType l ty'' else return ty''
  where
    splitArgs = do
      let (ts, es) = break isExprArg as
      let es'      = map (\(RExprArg e) -> e) es
      unless (all isExprArg es) $ lift $ throwGhc $
        ErrExprArgPos (locatedSrcSpan $ head es') errType
      return (ts, es')

    tcInfo = case synTyConDefn_maybe $ rtc_tc c of
      Nothing         -> return (Nothing, [])
      Just (tvs, rhs) -> do
        rta <- lookupTySyn $ rtc_tc c
        return $ case rta of
          Nothing               -> (Just (rTyVar <$> tvs, ofType rhs), [])
          Just (RTA tvs' evs b) -> (Just (tvs', ofUReft <$> val b), evs)

    errType = toUReft <$> RApp c as ps r

--------------------------------------------------------------------------------
-- Expansion Traversals Over Refinements ---------------------------------------
--------------------------------------------------------------------------------

expandRReft :: SrcSpan -> RReft -> ExpandM RReft
expandRReft l = traverse (expandReft l)

expandReft :: SrcSpan -> Reft -> ExpandM Reft
expandReft l (Reft (s, r)) = Reft . (s, ) <$> expandRefa l r

expandRefa :: SrcSpan -> Refa -> ExpandM Refa
expandRefa l = fmap Refa . expandPred l . raPred

expandPred :: SrcSpan -> Pred -> ExpandM Pred
expandPred l (PBexp e@EVar{}) = either id PBexp <$> expandExprVar l e
expandPred l (PBexp e@EApp{}) = either id PBexp <$> expandExprVar l e
expandPred l (PBexp e)        = PBexp <$> expandExpr l e
expandPred l (PAnd ps)        = PAnd <$> mapM (expandPred l) ps
expandPred l (POr  ps)        = POr <$> mapM (expandPred l) ps
expandPred l (PNot p)         = PNot <$> expandPred l p
expandPred l (PImp x y)       = PImp <$> expandPred l x <*> expandPred l y
expandPred l (PIff x y)       = PIff <$> expandPred l x <*> expandPred l y
expandPred l (PAtom b x y)    = PAtom b <$> expandExpr l x <*> expandExpr l y
expandPred l (PAll xss p)     = PAll xss <$> expandPred l p
expandPred _ p                = return p

expandExpr :: SrcSpan -> Expr -> ExpandM Expr
expandExpr l e@EVar{} = do
  result <- expandExprVar l e
  case result of
    Left  _  -> lift $ throwGhc (ErrPredInExpr l e :: Error)
    Right e' -> return e'
expandExpr l e@(EApp f _) = do
  result <- expandExprVar l e
  case result of
    Left  _  -> lift $ throwGhc (ErrPredInExpr (locatedSrcSpan f) e :: Error)
    Right e' -> return e'
expandExpr l (ENeg e    ) = ENeg <$> expandExpr l e
expandExpr l (EBin b x y) = EBin b <$> expandExpr l x <*> expandExpr l y
expandExpr l (EIte p x y) = EIte <$> expandPred l p <*> expandExpr l x <*> expandExpr l y
expandExpr l (ECst e s  ) = (`ECst` s) <$> expandExpr l e
expandExpr _ e            = return e

expandExprVar :: SrcSpan -> Expr -> ExpandM (Either Pred Expr)
expandExprVar l e = do
  es' <- mapM (expandExpr l) es
  ti  <- lookupInline $ val f
  case ti of
    Nothing -> return $ Right $ mkApp es'
    Just (TI args body) -> do
      unless (length args == length es') $ lift $ throwGhc $ mkErr args
      let su :: Subable a => a -> a
          su = subst $ mkSubst $ zip args es'
      return $ case body of
        Left  p -> Left  $ su p
        Right e -> Right $ su e
  where
    (f, es, mkApp) = case e of
      EVar v    -> (srcSpanLocated l v, [], const (EVar v))
      EApp f es -> (f, es, EApp f)
      otherwise -> error "expandExprVar: invalid argument"

    mkErr :: [Symbol] -> Error
    mkErr args =
      ErrInlineArgsCount (locatedSrcSpan f)
                         (pprint $ val f)
                         (length es) (length args)
                         e

--------------------------------------------------------------------------------
-- Compute Edges for Pred & Expr -----------------------------------------------
--------------------------------------------------------------------------------

predEdges :: Pred -> [Symbol]
predEdges = nub . go
  where
    go (PAnd ps)       = concatMap go ps
    go (POr  ps)       = concatMap go ps
    go (PImp p1 p2)    = go p1 ++ go p2
    go (PIff p1 p2)    = go p1 ++ go p2
    go (PAtom _ p1 p2) = exprEdges p1 ++ exprEdges p2
    go (PAll _ p)      = go p
    go (PNot p)        = go p
    go (PBexp e)       = exprEdges e
    go _               = []

exprEdges :: Expr -> [Symbol]
exprEdges = nub . go
  where
    go (EVar s)       = [s]
    go (EApp f es)    = val f : concatMap go es
    go (EBin _ e1 e2) = go e1 ++ go e2
    go (EIte p e1 e2) = predEdges p ++ go e1 ++ go e2
    go (ECst e _)     = go e
    go (ENeg e)       = go e
    go _              = []

--------------------------------------------------------------------------------
-- Graph-Based Expansion -------------------------------------------------------
--------------------------------------------------------------------------------

graphExpand :: (Eq k, Hashable k, Ord k')
            => (k -> k')
            -> (v -> [k'])
            -> ([k] -> Error)
            -> (k -> v -> ExpandM v)
            -> M.HashMap k v
            -> ExpandM (M.HashMap k v)

graphExpand mkKey mkEdges errCycle doExpand env
  | null cycles = M.fromList <$> mapM (\(k, v) -> (k, ) <$> doExpand k v) sorted
  | otherwise   = lift $ throwGhc $ errCycle . map fst <$> cycles
  where
    sorted =
      fst3 <$> flattenSCCs sccs
    cycles =
      mapMaybe (findCycle . flattenSCC) sccs
    sccs =
      stronglyConnCompG graph
    graph =
      graphFromEdgedVertices nodes
    nodes =
      map mkNode $ M.toList env
    mkNode (k, v) =
      ((k, v), mkKey k, mkEdges v)

