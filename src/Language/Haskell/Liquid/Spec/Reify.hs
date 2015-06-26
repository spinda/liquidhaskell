{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- TODO: There's likely a higher-level simplification that can be used to
--       express these transforms

module Language.Haskell.Liquid.Spec.Reify (
    ReifyM
  , runReifyM
  , lookupExprParams

  , reifyRTy
  , reifyRReft
  , reifyReft
  , reifyRefa
  , reifyPred
  , reifyExpr
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

import Control.Monad.Reader

import Data.List
import Data.Maybe
import Data.Monoid

import Text.Parsec.Pos

import Language.Fixpoint.Names
import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.RType (ExprParams(..))
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Spec.Env
import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------
-- Reify Monad -----------------------------------------------------------------
--------------------------------------------------------------------------------

newtype ReifyM a = ReifyM { unReifyM :: ReaderT SynEnv SpecM a }
                   deriving ( Functor, Applicative, Monad
                            , MonadIO
                            )

type SynEnv = NameEnv [Symbol]

instance GhcMonad ReifyM where
  getSession = ReifyM $ lift getSession
  setSession = ReifyM . lift . setSession

instance ExceptionMonad ReifyM where
  gcatch act handle = ReifyM $ do
    env <- ask
    lift $ runReifyM' act env
             `gcatch` \e -> runReifyM' (handle e) env
  gmask f = ReifyM $ do
    env <- ask
    lift $ gmask $ \ghc_restore ->
      runReifyM' (f $ reify_restore ghc_restore) env
    where
      reify_restore ghc_restore act = ReifyM $ do
         env <- ask
         lift $ ghc_restore $ runReifyM' act env

instance HasDynFlags ReifyM where
  getDynFlags = ReifyM $ lift getDynFlags


runReifyM :: ReifyM a -> ModGuts -> SpecM a
runReifyM act = runReifyM' act . mkSynEnv

runReifyM' :: ReifyM a -> SynEnv -> SpecM a
runReifyM' = runReaderT . unReifyM


mkSynEnv :: ModGuts -> SynEnv
mkSynEnv guts =
  mkNameEnv $ mapMaybe go $ mg_anns guts
  where
    go (Annotation (NamedTarget name) payload)
      | Just (ExprParams params) <- fromSerialized deserializeWithData payload =
        Just (name, map symbol params)
    go _ = Nothing


lookupExprParams :: TyCon -> ReifyM [Symbol]
lookupExprParams tc
  | isTypeSynonymTyCon tc = ReifyM $ do
    env <- ask
    return $ fromMaybe [] $ lookupNameEnv env (getName tc)
  | otherwise =
    return []

-- TODO: Typeclass this?
getWiredIns' :: ReifyM WiredIns
getWiredIns' = ReifyM $ lift getWiredIns

-- TODO: Typeclass this?
mkFreshInt' :: ReifyM Integer
mkFreshInt' = ReifyM $ lift mkFreshInt

--------------------------------------------------------------------------------
-- Reify RType -----------------------------------------------------------------
--------------------------------------------------------------------------------

reifyRTy :: Type -> ReifyM SpecType

reifyRTy (TyVarTy tv)  =
  return $ rVar tv

reifyRTy (AppTy t1 t2) =
  RAppTy <$> reifyRTy t1 <*> reifyRTy t2 <*> pure mempty

reifyRTy (TyConApp tc as) = go =<< getWiredIns'
  where
    go wis
      | tc == tc_Bind wis, [s, b, _a] <- as =
        invalidBind =<< reifyLocated reifySymbol s b
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
  es'    <- mapM (go <=< reifyTuple) =<< reifyList es
  params <- lookupExprParams tc
  if length params /= length es'
     then invalidExprArgs tc params es'
     else return $ subst (mkSubst (zip params $ map snd es')) a'
  where
    go (s, e) = (,) <$> reifySpan s <*> reifyExpr e
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
reifyPred ty = (`go` ty) =<< getWiredIns'
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
reifyExpr ty = (`go` ty) =<< getWiredIns'
  where
    go wis (TyConApp tc as)
      | tc == pc_ECon wis, [c] <- as =
        ECon <$> reifyConstant c
      | tc == pc_EBdr wis, [s] <- as =
        EVar <$> reifySymbol s
      | tc == pc_EParam wis, [s] <- as =
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


reifyConstant :: Type -> ReifyM Constant
reifyConstant ty = (`go` ty) =<< getWiredIns'
  where
    go wis (TyConApp tc [a])
      | tc == pc_I wis =
        I <$> reifyNat a
    go _ _ = malformed "constant" ty

reifyBrel :: Type -> ReifyM Brel
reifyBrel ty = (`go` ty) =<< getWiredIns'
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
reifyBop ty = (`go` ty) =<< getWiredIns'
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
  | Just dc <- isPromotedDataCon_maybe tc =
    return $ varSymbol $ dataConWorkId dc
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

reifyTuple :: Type -> ReifyM (Type, Type)
reifyTuple (TyConApp tc as)
  | tc == promotedTupleDataCon BoxedTuple 2, [_, _, x, y] <- as =
    return (x, y)
reifyTuple ty =
  malformed "type-level tuple" ty


reifyLocated :: (Type -> ReifyM a) -> Type -> Type -> ReifyM (Located a)
reifyLocated f s x = do
  (st, ed) <- reifySpan s
  x'       <- f x
  return $ Loc st ed x'

reifySpan :: Type -> ReifyM (SourcePos, SourcePos)
reifySpan ty = (`go` ty) =<< getWiredIns'
  where
    go wis (TyConApp tc [filename, startLine, startCol, endLine, endCol])
      | tc == pc_Span wis = do
        filename'  <- reifyString filename
        startLine' <- fromIntegral <$> reifyNat startLine
        startCol'  <- fromIntegral <$> reifyNat startCol
        endLine'   <- fromIntegral <$> reifyNat endLine
        endCol'    <- fromIntegral <$> reifyNat endCol
        return ( newPos filename' startLine' startCol'
               , newPos filename' endLine'   endCol'
               )
    go _ _ = malformed "location annotation" ty


reifyBind :: Type -> ReifyM (Symbol, Type)
reifyBind ty = (`go` ty) =<< getWiredIns'
  where
    go wis (TyConApp tc [_s, b, a])
      | tc == tc_Bind wis = (, a) <$> reifySymbol b
    go _ _ = ((, ty) . tempSymbol "db") <$> mkFreshInt'


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

malformed :: GhcMonad m => String -> Type -> m a
malformed desc ty = panic $
  "Malformed LiquidHaskell " ++ desc ++ " encoding: " ++ dumpType ty

invalidBind :: GhcMonad m => Located Symbol -> m a
invalidBind lb = panic $
  "Bind cannot appear at this location: " ++ show lb

invalidExprArgs :: GhcMonad m => TyCon -> [Symbol] -> [((SourcePos, SourcePos), Expr)] -> m a
invalidExprArgs tc params es = panic $
  "Type synonym " ++ showPpr tc ++
  " expects " ++ show expected ++
  " expression arguments but has been given " ++ show actual ++
  ", at the instantiation from " ++ show st ++ " to " ++ show ed
  where
    expected = length params
    actual   = length es
    (st, ed)
      | actual > expected = fst (es !! expected)
      | otherwise         = fst (last es)

dumpType :: Type -> String
dumpType (TyVarTy tv) = "(TyVarTy " ++ showPpr tv ++ ")"
dumpType (AppTy t1 t2) = "(AppTy " ++ dumpType t1 ++ " " ++ dumpType t2 ++ ")"
dumpType (TyConApp tc as) = "(TyConApp " ++ showPpr tc ++ " [" ++ intercalate ", " (map dumpType as) ++ "])"
dumpType (FunTy t1 t2) = "(FunTy " ++ dumpType t1 ++ " " ++ dumpType t2 ++ ")"
dumpType (ForAllTy tv ty) = "(ForAllTy " ++ showPpr tv ++ " " ++ dumpType ty ++ ")"
dumpType (LitTy lit) = "(LitTy " ++ showPpr lit ++ ")"

