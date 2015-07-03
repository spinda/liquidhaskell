{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- TODO: There's likely a higher-level simplification that can be used to
--       express these transforms

module Language.Haskell.Liquid.Spec.Reify (
    ReifyM
  , runReifyM
  , lookupExprParams
  , getTypecheckedModule'

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
import HscMain
import HscTypes
import IdInfo
import MonadUtils
import Name
import NameEnv
import Panic
import PrelNames
import RdrName
import Serialized
import TcRnTypes
import TyCon
import Type
import TypeRep
import TysWiredIn
import Unique
import Var
import VarEnv

import qualified Outputable as Out

import Control.Arrow
import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Monoid

import qualified Data.HashMap.Strict as M

import Text.Parsec.Pos

import Language.Fixpoint.Names
import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.RType (ExprParams(..))
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------
-- ReifyM Monad ----------------------------------------------------------------
--------------------------------------------------------------------------------

newtype ReifyM a = ReifyM { unReifyM :: StateT ReifyState Ghc a }
                   deriving (Functor, Applicative, Monad, MonadIO)

data ReifyState = RS { rs_freshInt   :: Integer
                     , rs_tcModule   :: TypecheckedModule
                     , rs_wiredIns   :: WiredIns
                     , rs_exprParams :: NameEnv [Symbol]
                     , rs_inlines    :: VarEnv TInline
                     , rs_freeSyms   :: M.HashMap Symbol Var
                     }


runReifyM :: ReifyM a -> TypecheckedModule -> WiredIns -> NameEnv [Symbol] -> VarEnv TInline -> Ghc (a, [(Symbol, Var)])
runReifyM act tm wiredIns exprParams inlines = do
  (x, s) <- runStateT (unReifyM act) initState
  return (x, M.toList $ rs_freeSyms s)
  where
    initState = RS 0 tm wiredIns exprParams inlines mempty


liftGhc :: Ghc a -> ReifyM a
liftGhc = ReifyM . lift

mkFreshInt :: ReifyM Integer
mkFreshInt = ReifyM $ do
  state@(RS { rs_freshInt = freshInt }) <- get
  put $ state { rs_freshInt = freshInt + 1 }
  return freshInt

getTypecheckedModule' :: ReifyM TypecheckedModule
getTypecheckedModule' = ReifyM $ gets rs_tcModule

lookupTyThing :: Name -> ReifyM (Maybe TyThing)
lookupTyThing name = do
  env <- (tcg_type_env . fst . tm_internals_) <$> getTypecheckedModule'
  let local = lookupTypeEnv env name
  case local of
    Just thing -> return (Just thing)
    Nothing    -> liftGhc $ lookupName name

getWiredIns :: ReifyM WiredIns
getWiredIns = ReifyM $ gets rs_wiredIns

lookupExprParams :: TyCon -> ReifyM [Symbol]
lookupExprParams tc = ReifyM $ do
  env <- gets rs_exprParams
  return $ fromMaybe [] $ lookupNameEnv env $ getName tc

lookupInline :: Located Symbol -> ReifyM (Maybe TInline)
lookupInline s = do
  hscEnv   <- liftGhc getSession 
  rdr      <- liftIO $ hscParseIdentifier hscEnv $ symbolString $ val s
  gre      <- (tcg_rdr_env . fst . tm_internals_) <$> getTypecheckedModule'
  let names = map gre_name (lookupGRE_RdrName (unLoc rdr) gre)
  things   <- mapMaybeM lookupTyThing names
  let var = listToMaybe $ mapMaybe findVar things
  case var of
    Nothing  -> return Nothing
    Just key -> ReifyM $ gets (flip lookupVarEnv key . rs_inlines)
  where
    findVar (AnId var) = Just var
    findVar _          = Nothing

    catchNotInScope :: SourceError -> Ghc [Name]
    catchNotInScope _ = return []

visitFreeSym :: Symbol -> Var -> ReifyM ()
visitFreeSym sym var = ReifyM $ modify $ \st ->
  st { rs_freeSyms = M.insert sym var $ rs_freeSyms st }

--------------------------------------------------------------------------------
-- Reify RType -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRTy :: Type -> ReifyM SpecType

reifyRTy (TyVarTy tv)  =
  return $ rVar tv

reifyRTy (AppTy t1 t2) =
  RAppTy <$> reifyRTy t1 <*> reifyRTy t2 <*> pure mempty

reifyRTy (TyConApp tc as) = go =<< getWiredIns
  where
    go wis
      | tc == tc_Bind wis, [b, _a] <- as =
        invalidBind =<< traverse reifySymbol =<< reifyLocated b
      | tc == tc_Refine wis, [a, b, p] <- as =
        strengthen <$> reifyRTy a <*> reifyRReft b p
      | tc == tc_ExprArgs wis, [a, es] <- as =
        reifyExprArgs a es
      | Just (tenv, rhs, as') <- tcExpandTyCon_maybe tc as =
        reifyRTy $ mkAppTys (substTy (mkTopTvSubst tenv) rhs) as'
      | otherwise =
        rApp tc <$> mapM reifyRTy as <*> pure [] <*> pure mempty

reifyRTy (FunTy i o) = do
  (b, i') <- reifyBind i
  RFun b <$> reifyRTy i' <*> reifyRTy o <*> pure mempty

reifyRTy (ForAllTy tv ty) = do
  RAllT (rTyVar tv) <$> reifyRTy ty

reifyRTy ty@(LitTy _) =
  malformed "type" ty


reifyExprArgs :: Type -> Type -> ReifyM SpecType
reifyExprArgs a@(TyConApp tc _) es = do
  a'     <- reifyRTy a
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
reifyPred = applyInlines <=< reifyPred'

reifyPred' :: Type -> ReifyM Pred
reifyPred' ty = (`go` ty) =<< getWiredIns
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


applyInlines :: Pred -> ReifyM Pred
applyInlines (PBexp e@(EApp f es)) = do
  inline <- lookupInline f
  case inline of
    Just (TI args (Left body)) -> do
      unless (length args == length es) (invalidInlineArgs f args es)
      es' <- mapM applyInlines' es
      return $ subst (mkSubst $ zip args es') body
    _ ->
      PBexp <$> applyInlines' e
applyInlines (PBexp e    ) = PBexp <$> applyInlines' e
applyInlines (PAnd ps    ) = PAnd <$> mapM applyInlines ps
applyInlines (POr  ps    ) = POr <$> mapM applyInlines ps
applyInlines (PNot p     ) = PNot <$> applyInlines p
applyInlines (PImp x y   ) = PImp <$> applyInlines x <*> applyInlines y
applyInlines (PIff x y   ) = PIff <$> applyInlines x <*> applyInlines y
applyInlines (PAtom b x y) = PAtom b <$> applyInlines' x <*> applyInlines' y
applyInlines (PAll xss p ) = PAll xss <$> applyInlines p
applyInlines p             = return p

applyInlines' :: Expr -> ReifyM Expr
applyInlines' (EApp f es) = do
  inline <- lookupInline f
  es'    <- mapM applyInlines' es
  case inline of
    Nothing ->
      return $ EApp f es'
    Just (TI _ (Left _)) ->
      liftGhc $ panic "TODO"
    Just (TI args (Right body)) -> do
      unless (length args == length es) (invalidInlineArgs f args es)
      return $ subst (mkSubst $ zip args es') body
applyInlines' (ENeg e    ) = ENeg <$> applyInlines' e
applyInlines' (EBin b x y) = EBin b <$> applyInlines' x <*> applyInlines' y
applyInlines' (EIte p x y) = EIte <$> applyInlines p <*> applyInlines' x <*> applyInlines' y
applyInlines' (ECst e s  ) = (`ECst` s) <$> applyInlines' e
applyInlines' e            = return e

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
      | tc == pc_ECtr wis, [_, dc] <- as =
        (EVar . val) <$> (traverse reifyDataCon =<< reifyLocated dc)
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
-- Reify Data Constructor ------------------------------------------------------
--------------------------------------------------------------------------------

reifyDataCon :: Type -> ReifyM Symbol
reifyDataCon (TyConApp tc _)
  | Just dc <- isPromotedDataCon_maybe tc = do
    let var = dataConWorkId dc
    let sym = varSymbol var
    visitFreeSym sym var
    return sym
reifyDataCon ty =
  malformed "data constructor" ty

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
    go wis (TyConApp tc [b, a])
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

invalidBind :: Located Symbol -> ReifyM a
invalidBind lb = liftGhc $ panic $
  "Bind cannot appear at this location: " ++ show lb

invalidExprArgs :: TyCon -> [Symbol] -> [Located Expr] -> ReifyM a
invalidExprArgs tc params es = liftGhc $ panic $
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
invalidEAppHead e = liftGhc $ panic $
  "Expression does not take any parameters, and thus cannot begin an application: " ++ showpp e

invalidInlineArgs :: LocSymbol -> [Symbol] -> [Expr] -> ReifyM a
invalidInlineArgs f args es = liftGhc $ panic $
  "Inline " ++ showpp f ++
  " expects " ++ show (length args) ++
  " arguments but has been given " ++ show (length es)


dumpType :: Type -> String
dumpType (TyVarTy tv) = "(TyVarTy " ++ showPpr tv ++ ")"
dumpType (AppTy t1 t2) = "(AppTy " ++ dumpType t1 ++ " " ++ dumpType t2 ++ ")"
dumpType (TyConApp tc as) = "(TyConApp " ++ showPpr tc ++ " [" ++ intercalate ", " (map dumpType as) ++ "])"
dumpType (FunTy t1 t2) = "(FunTy " ++ dumpType t1 ++ " " ++ dumpType t2 ++ ")"
dumpType (ForAllTy tv ty) = "(ForAllTy " ++ showPpr tv ++ " " ++ dumpType ty ++ ")"
dumpType (LitTy lit) = "(LitTy " ++ showPpr lit ++ ")"

