{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- TODO: There's likely a higher-level simplification that can be used to
--       express these transforms

module Language.Haskell.Liquid.Spec.Reify (
    ReifyM
  , runReifyM
  , lookupExprParams

  , reifyRTy
  ) where

import GHC hiding (Located)

import Annotations
import BasicTypes (TupleSort(..))
import ConLike
import DataCon
import DynFlags
import Exception
import FastString
import HscTypes
import IdInfo
import MonadUtils
import Name
import NameEnv
import Panic
import PrelNames
import Serialized
import TyCon
import Type
import TypeRep
import TysWiredIn
import Unique
import Var

import qualified Outputable as Out

import Control.Arrow
import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Monoid

import qualified Data.HashSet        as S
import qualified Data.HashMap.Strict as M

import Text.Parsec.Pos

import Language.Fixpoint.Names
import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.TH.Types (ExprParams(..))

import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------
-- ReifyM Monad ----------------------------------------------------------------
--------------------------------------------------------------------------------

newtype ReifyM a = ReifyM { unReifyM :: StateT ReifyState Ghc a }
                   deriving (Functor, Applicative, Monad, MonadIO)

data ReifyState = RS { rs_freshInt   :: !Integer
                     , rs_bindScope  :: !(S.HashSet Symbol)
                     , rs_wiredIns   :: !WiredIns
                     , rs_rtEnv      :: !RTEnv
                     , rs_exprParams :: !(NameEnv [Symbol])
                     , rs_inlines    :: !(M.HashMap Var TInline)
                     , rs_meas       :: !(M.HashMap Var SpecMeasure)
                     , rs_freeSyms   :: !(M.HashMap Symbol Var)
                     }


runReifyM :: ReifyM a
          -> WiredIns
          -> RTEnv
          -> NameEnv [Symbol]
          -> M.HashMap Var TInline
          -> M.HashMap Var SpecMeasure
          -> Ghc (a, M.HashMap Symbol Var)
runReifyM act wiredIns rtEnv exprParams inlines meas = do
  (x, s) <- runStateT (unReifyM act) initState
  return (x, rs_freeSyms s)
  where
    initState   = RS 0 mempty wiredIns rtEnv exprParams' inlines meas mempty
    exprParams' = plusNameEnv exprParams $
      mkNameEnv $ map (getName *** rtEArgs) $ M.toList rtEnv


liftGhc :: Ghc a -> ReifyM a
liftGhc = ReifyM . lift

mkFreshInt :: ReifyM Integer
mkFreshInt = ReifyM $ do
  state@(RS { rs_freshInt = freshInt }) <- get
  put $ state { rs_freshInt = freshInt + 1 }
  return freshInt

withBindScope :: [Symbol] -> ReifyM a -> ReifyM a
withBindScope binds act = ReifyM $ do
  state@RS{..} <- get
  put $ state { rs_bindScope = S.union (S.fromList binds) rs_bindScope }
  result <- unReifyM act
  put state
  return result

isBindInScope :: Symbol -> ReifyM Bool
isBindInScope bind = ReifyM $ gets (S.member bind . rs_bindScope)

getWiredIns :: ReifyM WiredIns
getWiredIns = ReifyM $ gets rs_wiredIns

lookupRTAlias :: TyCon -> ReifyM (Maybe (RTAlias RTyVar SpecType))
lookupRTAlias tc = ReifyM $ gets (M.lookup tc . rs_rtEnv)

lookupExprParams :: TyCon -> ReifyM [Symbol]
lookupExprParams tc = ReifyM $ do
  env <- gets rs_exprParams
  return $ fromMaybe [] $ lookupNameEnv env $ getName tc

lookupInline :: Var -> ReifyM (Maybe TInline)
lookupInline var = ReifyM $ gets (M.lookup var . rs_inlines)

lookupMeasure :: Var -> ReifyM (Maybe SpecMeasure)
lookupMeasure var = ReifyM $ gets (M.lookup var . rs_meas)

lookupVar :: Symbol -> ReifyM (Maybe Var)
lookupVar s = liftGhc $ do
  names  <- ghandle catchNotInScope $ parseName $ symbolString s
  things <- mapMaybeM lookupName names
  return $ listToMaybe $ mapMaybe tyThingId_maybe things
  where
    catchNotInScope :: SourceError -> Ghc [Name]
    catchNotInScope _ = return []

visitFreeSym :: Symbol -> Var -> ReifyM ()
visitFreeSym sym var = ReifyM $ modify $ \st ->
  st { rs_freeSyms = M.insert sym var $ rs_freeSyms st }

--------------------------------------------------------------------------------
-- Reify RType -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRTy :: Type -> ReifyM SpecType
reifyRTy ty = do
  ty' <- reifyRTy' ty
  let rep = toRTypeRep ty'
  withBindScope (ty_binds rep) (resolveRTy ty')


reifyRTy' :: Type -> ReifyM SpecType

reifyRTy' (TyVarTy tv)  =
  return $ rVar tv

reifyRTy' (AppTy t1 t2) =
  RAppTy <$> reifyRTy' t1 <*> reifyRTy' t2 <*> pure mempty

reifyRTy' (TyConApp tc as) = go =<< getWiredIns
  where
    go wis
      | tc == tc_Bind wis, [_, b, _a] <- as =
        invalidBind =<< traverse reifySymbol =<< reifyLocated b
      | tc == tc_Refine wis, [_, a, b, p] <- as =
        strengthen <$> reifyRTy' a <*> reifyRReft b p
      | tc == tc_ExprArgs wis, [_, a, es] <- as =
        reifyExprArgs a es
      | isTypeSynonymTyCon tc =
        reifyTySynApp tc as
      | otherwise =
        rApp tc <$> mapM reifyRTy' as <*> pure [] <*> pure mempty

reifyRTy' (FunTy i o) = do
  (b, i') <- reifyBind i
  RFun b <$> reifyRTy' i' <*> reifyRTy' o <*> pure mempty

reifyRTy' (ForAllTy tv ty) = do
  RAllT (rTyVar tv) <$> reifyRTy' ty

reifyRTy' ty@(LitTy _) =
  malformed "type" ty


reifyTySynApp :: TyCon -> [Type] -> ReifyM SpecType
reifyTySynApp tc as = do
  rtAlias <- lookupRTAlias tc
  case rtAlias of
    Just (RTA targs _ body) -> do
      as' <- mapM reifyRTy' as
      let ats = zipWith (\a t -> (a, toRSort t, t)) targs as'
      return $ subsTyVars_meet ats body
    Nothing ->
      let Just (tenv, rhs, as') = tcExpandTyCon_maybe tc as
      in  reifyRTy' (mkAppTys (substTy (mkTopTvSubst tenv) rhs) as')

-- TODO: Error out when no expression arguments are given and some are needed
reifyExprArgs :: Type -> Type -> ReifyM SpecType
reifyExprArgs a@(TyConApp tc _) es = do
  a'     <- reifyRTy' a
  es'    <- mapM (traverse reifyExpr <=< reifyLocated) =<< reifyList es
  params <- lookupExprParams tc
  if length params /= length es'
     then invalidExprArgs tc params es'
     else return $ subst (mkSubst (zip params $ map val es')) a'
-- TODO: Report proper message when trying to apply expr args to non-type
--       constructor
reifyExprArgs a _ =
  malformed "expression args instantiation" a

--------------------------------------------------------------------------------
-- Reify RReft -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRReft :: Type -> Type -> ReifyM RReft
reifyRReft b r = do
  r' <- reifyReft b r
  return $ (mempty :: RReft) { ur_reft = r' }


reifyReft :: Type -> Type -> ReifyM Reft
reifyReft b r = do
  b' <- reifySymbol b
  r' <- reifyRefa r
  return $ Reft (b', r')


reifyRefa :: Type -> ReifyM Refa
reifyRefa = fmap Refa . reifyPred

--------------------------------------------------------------------------------
-- Reify Pred -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyPred :: Type -> ReifyM Pred
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

reifyExpr :: Type -> ReifyM Expr
reifyExpr ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc as)
      | tc == pc_ECon wis, [c] <- as =
        ECon <$> reifyConstant c
      | tc == pc_EVar wis, [s] <- as =
        EVar <$> reifySymbol s
      | tc == pc_EParam wis, [s] <- as =
        EVar <$> reifySymbol s
      | tc == pc_EApp wis, [e, es] <- as =
        reifyEApp e es
      | tc == pc_ENeg wis, [e] <- as =
        ENeg <$> reifyExpr e
      | tc == pc_EBin wis, [bop, e1, e2] <- as =
        EBin <$> reifyBop bop <*> reifyExpr e1 <*> reifyExpr e2
      | tc == pc_EIte wis, [pred, e1, e2] <- as =
        EIte <$> reifyPred pred <*> reifyExpr e1 <*> reifyExpr e2
      | tc == pc_EBot wis, [] <- as =
        return EBot
    go _ _ = malformed "expression" ty

reifyEApp :: Type -> Type -> ReifyM Expr
reifyEApp e es = do
  e'  <- traverse reifyExpr =<< reifyLocated e
  es' <- traverse reifyExpr =<< reifyList es
  case val e' of
    EVar v      -> return $ EApp (const v <$> e') es'
    EApp v es'' -> return $ EApp v (es'' ++ es')
    _           -> invalidEAppHead e'


reifyConstant :: Type -> ReifyM Constant
reifyConstant ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [a])
      | tc == pc_I wis =
        I <$> reifyNat a
    go _ _ = malformed "constant" ty

reifyBrel :: Type -> ReifyM Brel
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

reifyBop :: Type -> ReifyM Bop
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
-- Resolve Pred & Expr Applications --------------------------------------------
--------------------------------------------------------------------------------

resolveRTy :: SpecType -> ReifyM SpecType
resolveRTy = mapReftM resolveRReft

resolveRReft :: RReft -> ReifyM RReft
resolveRReft = traverse resolveReft

resolveReft :: Reft -> ReifyM Reft
resolveReft (Reft (s, r)) = Reft . (s, ) <$> resolveRefa r

resolveRefa :: Refa -> ReifyM Refa
resolveRefa = fmap Refa . resolvePred . raPred


-- FIXME: Carry location through EVar (requires Fixpoint change)
resolvePred :: Pred -> ReifyM Pred
resolvePred (PBexp (EVar v   )) = resolvePred $ PBexp $ EApp (dummyLoc v) []
resolvePred (PBexp (EApp f es)) = either id PBexp <$> resolveVar f es
resolvePred (PBexp e    )       = PBexp <$> resolveExpr e
resolvePred (PAnd ps    )       = PAnd <$> mapM resolvePred ps
resolvePred (POr  ps    )       = POr <$> mapM resolvePred ps
resolvePred (PNot p     )       = PNot <$> resolvePred p
resolvePred (PImp x y   )       = PImp <$> resolvePred x <*> resolvePred y
resolvePred (PIff x y   )       = PIff <$> resolvePred x <*> resolvePred y
resolvePred (PAtom b x y)       = PAtom b <$> resolveExpr x <*> resolveExpr y
resolvePred (PAll xss p )       = PAll xss <$> resolvePred p
resolvePred p                   = return p

-- FIXME: Carry location through EVar (requires Fixpoint change)
resolveExpr :: Expr -> ReifyM Expr
resolveExpr (EVar v)    = resolveExpr $ EApp (dummyLoc v) []
resolveExpr (EApp f es) = do
  resolved <- resolveVar f es
  case resolved of
    Left  _ -> predInExprCtxt f
    Right e -> return e
resolveExpr (ENeg e    ) = ENeg <$> resolveExpr e
resolveExpr (EBin b x y) = EBin b <$> resolveExpr x <*> resolveExpr y
resolveExpr (EIte p x y) = EIte <$> resolvePred p <*> resolveExpr x <*> resolveExpr y
resolveExpr (ECst e s  ) = (`ECst` s) <$> resolveExpr e
resolveExpr e            = return e


resolveVar :: LocSymbol -> [Expr] -> ReifyM (Either Pred Expr)
resolveVar f es = go_bind =<< mapM resolveExpr es
  where
    go_bind es' = do
      bind <- isBindInScope $ val f
      if bind
         then return $ Right $ eVarOrApp f es
         else go_var es'
    go_var es' = do
      mv <- sequence <$> traverse lookupVar f
      case mv of
        Nothing ->
          return $ Right $ eVarOrApp f es'
        Just var -> do
          when (isDataConId $ val var) $
            visitFreeSym (varSymbol $ val var) (dataConWorkId $ idDataCon $ val var)
          go_inline var es'
    go_inline var es' = do
      inline <- lookupInline $ val var
      case inline of
        Just (TI args body) -> do
          unless (length args == length es') (invalidInlineArgs f args es')
          let su :: Subable a => a -> a
              su = subst $ mkSubst $ zip args es'
          return $ case body of
            Left  p -> Left  $ su p
            Right e -> Right $ su e
        Nothing -> Right <$> go_measure var es'
    go_measure var es' = do
      measure <- lookupMeasure $ val var
      return $ case measure of
        Just M{..} ->
          eVarOrApp (const (val name) <$> var) es'
        Nothing -> go_fun var es'
    go_fun var es'
      | isFunVar $ val var =
        EApp (varSymbol <$> var) es'
      | otherwise =
        eVarOrApp (varSymbol <$> var) es'


isFunVar :: Var -> Bool
isFunVar v = isDataConId v && not (null as) && isNothing tf
  where
    (as, t) = splitForAllTys $ varType v 
    tf      = splitFunTy_maybe t

eVarOrApp :: LocSymbol -> [Expr] -> Expr
eVarOrApp v [] = EVar (val v)
eVarOrApp f es = EApp f es

--------------------------------------------------------------------------------
-- Reify Components ------------------------------------------------------------
--------------------------------------------------------------------------------

reifyList :: Type -> ReifyM [Type]
reifyList (TyConApp tc as)
  | tc `hasKey` consDataConKey, [_, x, xs] <- as =
    (x:) <$> reifyList xs
  | tc `hasKey` nilDataConKey, [_] <- as =
    return []
reifyList ty =
  malformed "type-level list" ty


reifyLocated :: Type -> ReifyM (Located Type)
reifyLocated ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [_, s, x])
      | tc == pc_TyLocated wis = do
        (start, end) <- reifySpan s
        return $ Loc start end x
    go _ _ = malformed "location annotation" ty

reifySpan :: Type -> ReifyM (SourcePos, SourcePos)
reifySpan ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [start, end])
      | tc == pc_TySpan wis =
        (,) <$> reifyPos start <*> reifyPos end
    go _ _ = malformed "source span" ty

reifyPos :: Type -> ReifyM SourcePos
reifyPos ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [name, line, col])
      | tc == pc_TyPos wis = 
        newPos <$> reifyString name
               <*> (fromIntegral <$> reifyNat line)
               <*> (fromIntegral <$> reifyNat col)
    go _ _ = malformed "source position" ty


reifyBind :: Type -> ReifyM (Symbol, Type)
reifyBind ty = (`go` ty) =<< getWiredIns
  where
    go wis (TyConApp tc [_, b, a])
      | tc == tc_Bind wis = (, a) <$> (reifySymbol =<< (val <$> reifyLocated b))
    go _ _ = ((, ty) . tempSymbol "db") <$> mkFreshInt


reifyString :: Type -> ReifyM String
reifyString (LitTy (StrTyLit s)) = return $ unpackFS s
reifyString ty                   = malformed "symbol" ty

reifySymbol :: Type -> ReifyM Symbol
reifySymbol = fmap symbol . reifyString

reifyNat :: Type -> ReifyM Integer
reifyNat (LitTy (NumTyLit n)) = return n
reifyNat ty                   = malformed "natural number" ty

--------------------------------------------------------------------------------
-- Error Messages --------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Review and revise error messages (across the board, not just here)

malformed :: String -> Type -> ReifyM a
malformed desc ty = liftGhc $ panic $
  "Malformed LiquidHaskell " ++ desc ++ " encoding: " ++ dumpType ty

dumpType :: Type -> String
dumpType (TyVarTy tv) = "(TyVarTy " ++ showPpr tv ++ ")"
dumpType (AppTy t1 t2) = "(AppTy " ++ dumpType t1 ++ " " ++ dumpType t2 ++ ")"
dumpType (TyConApp tc as) = "(TyConApp " ++ showPpr tc ++ " [" ++ intercalate ", " (map dumpType as) ++ "])"
dumpType (FunTy t1 t2) = "(FunTy " ++ dumpType t1 ++ " " ++ dumpType t2 ++ ")"
dumpType (ForAllTy tv ty) = "(ForAllTy " ++ showPpr tv ++ " " ++ dumpType ty ++ ")"
dumpType (LitTy lit) = "(LitTy " ++ showPpr lit ++ ")"


invalidBind :: LocSymbol -> ReifyM a
invalidBind lb = liftGhc $ pgmError $
  "Bind cannot appear at this location: " ++ show lb

predInExprCtxt :: LocSymbol -> ReifyM a
predInExprCtxt ls = liftGhc $ pgmError $
  "Predicate application cannot appear in expression context: " ++ show ls

invalidExprArgs :: TyCon -> [Symbol] -> [Located Expr] -> ReifyM a
invalidExprArgs tc params es = liftGhc $ pgmError $
  "Type synonym " ++ showPpr tc ++
  " expects " ++ show expected ++
  " expression arguments but has been given " ++ show actual ++
  ", at the instantiation from " ++ show st ++ " to " ++ show ed
  where
    expected = length params
    actual   = length es
    (st, ed)
      | actual > expected = (loc &&& locE) (es !! expected)
      | otherwise         = (loc &&& locE) (last es)

invalidEAppHead :: Located Expr -> ReifyM a
invalidEAppHead e = liftGhc $ pgmError $
  "Expression does not take any parameters, and thus cannot begin an application: " ++ showpp e

invalidInlineArgs :: LocSymbol -> [Symbol] -> [Expr] -> ReifyM a
invalidInlineArgs f args es = liftGhc $ pgmError $
  "Inline " ++ showpp f ++
  " expects " ++ show (length args) ++
  " arguments but has been given " ++ show (length es)

