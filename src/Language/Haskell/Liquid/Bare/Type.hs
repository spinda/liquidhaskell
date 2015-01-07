{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Bare.Type (
    ofBareType
  , ofMeaSort
  , ofBSort

  , ofBPVar

  , mkSpecType
  , mkSpecType'
  ) where

import BasicTypes
import TyCon
import Type (expandTypeSynonyms)
import TysWiredIn

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader hiding (forM)
import Control.Monad.State hiding (forM)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Text.Parsec.Pos
import Text.Printf

import qualified Control.Exception as Ex 
import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Misc (errorstar)
import Language.Fixpoint.Types (Expr(..), Reftable, Symbol, meet, mkSubst, subst, symbol)

import Language.Haskell.Liquid.GhcMisc (sourcePosSrcSpan)
import Language.Haskell.Liquid.Misc (secondM)
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Bare.Env
import Language.Haskell.Liquid.Bare.Expand
import Language.Haskell.Liquid.Bare.Lookup
import Language.Haskell.Liquid.Bare.Resolve

--------------------------------------------------------------------------------

type TypeM r = ReaderT (TypeEnv r) BareM

data TypeEnv r = TE { te_lookupTyCon :: LocSymbol -> BareM TyCon
                    , te_resolveReft :: r -> BareM r
                    , te_appRTAlias  :: BRType r -> RTAlias RTyVar SpecType -> TypeM r (RRType r)
                    }

lookupTyCon :: LocSymbol -> TypeM r TyCon
lookupTyCon lc
  = do f <- asks te_lookupTyCon
       lift $ f lc

resolveReft :: r -> TypeM r r
resolveReft r
  = do f <- asks te_resolveReft
       lift $ f r

appRTAlias :: BRType r -> RTAlias RTyVar SpecType -> TypeM r (RRType r)
appRTAlias t rta
  = do f <- asks te_appRTAlias
       f t rta

--------------------------------------------------------------------------------

ofBareType :: SourcePos -> BareType -> BareM SpecType
ofBareType l
  = ofBRType (ofBareType_env l)

ofMeaSort :: BareType -> BareM SpecType
ofMeaSort
  = ofBRType ofMeaSort_env

ofBSort :: BSort -> BareM RSort
ofBSort
  = ofBRType ofBSort_env

ofBSort_inner :: BSort -> TypeM r RSort
ofBSort_inner t
  = do lookupTyCon <- asks te_lookupTyCon
       let env = ofBSort_env { te_lookupTyCon = lookupTyCon }
       lift $ ofBRType env t


ofBareType_env :: SourcePos -> TypeEnv RReft
ofBareType_env l
  = TE { te_lookupTyCon = lookupGhcTyCon
       , te_resolveReft = expandReft l >=> resolve l
       , te_appRTAlias  = expandRTAliasApp
       }

ofMeaSort_env :: TypeEnv RReft
ofMeaSort_env
  = TE { te_lookupTyCon = lookupGhcTyCon
       , te_resolveReft = return
       , te_appRTAlias  = failRTAliasApp
       }

ofBSort_env :: TypeEnv ()
ofBSort_env
  = TE { te_lookupTyCon = lookupGhcTyCon
       , te_resolveReft = return
       , te_appRTAlias  = failRTAliasApp
       }

--------------------------------------------------------------------------------

ofBRType :: (PPrint r, Reftable r) => TypeEnv r -> BRType r -> BareM (RRType r)
ofBRType env t
  = runReaderT (ofBRType' t) env

ofBRType' :: (PPrint r, Reftable r) => BRType r -> TypeM r (RRType r)
ofBRType' t@(RApp _ _ _ _)
  = ofRApp t
ofBRType' (RVar a r)
  = RVar (symbolRTyVar a) <$> resolveReft r
ofBRType' (RFun x t1 t2 _)
  = rFun x <$> ofBRType' t1 <*> ofBRType' t2
ofBRType' (RAppTy t1 t2 r)
  = RAppTy <$> ofBRType' t1 <*> ofBRType' t2 <*> resolveReft r
ofBRType' (RAllE x t1 t2)
  = RAllE x <$> ofBRType' t1 <*> ofBRType' t2
ofBRType' (REx x t1 t2)
  = REx x <$> ofBRType' t1 <*> ofBRType' t2
ofBRType' (RAllT a t)
  = RAllT (symbolRTyVar a) <$> ofBRType' t
ofBRType' (RAllP a t)
  = RAllP <$> ofBPVar_inner a <*> ofBRType' t
ofBRType' (RAllS x t)
  = RAllS x <$> ofBRType' t
ofBRType' (ROth s)
  = return $ ROth s
ofBRType' (RExprArg e)
  = return $ RExprArg e
ofBRType' (RHole r)
  = RHole <$> resolveReft r
ofBRType' (RRTy _ _ _ _)
  = errorstar "Bare.Type.ofBRType' called on RRTy" 

--------------------------------------------------------------------------------

ofBPVar :: BPVar -> BareM RPVar
ofBPVar
  = mapM_pvar ofBSort

ofBPVar_inner :: BPVar -> TypeM r RPVar
ofBPVar_inner
  = mapM_pvar ofBSort_inner

--------------------------------------------------------------------------------

ofRApp :: (PPrint r, Reftable r) => BRType r -> TypeM r (RRType r)
ofRApp t@(RApp lc@(Loc _ c) ts rs r)
  = do env <- gets (typeAliases.rtEnv)
       case M.lookup c env of
         Just rta ->
           appRTAlias t rta
         Nothing
           | isList c && length ts == 1 ->
             app listTyCon
           | isTuple c ->
             app $ tupleTyCon BoxedTuple (length ts)
           | otherwise ->
             lookupTyCon lc >>= app
  where
    app tc
      = do r' <- resolveReft r
           (bareTCApp r' tc) <$> (mapM ofRef rs) <*> (mapM ofBRType' ts)
ofRApp _
  = errorstar  "Bare.Type.ofRApp called on invalid input" 

ofRef (RPropP ss r)
  = RPropP <$> mapM ofSyms ss <*> resolveReft r
ofRef (RProp ss t)
  = RProp <$> mapM ofSyms ss <*> ofBRType' t
ofRef (RHProp _ _)
  = errorstar "TODO:EFFECTS:ofRef"

ofSyms
  = secondM ofBSort_inner

--------------------------------------------------------------------------------

mkSpecType :: SourcePos -> BareType -> BareM SpecType
mkSpecType l t
  = mkSpecType' l (ty_preds $ toRTypeRep t) t

mkSpecType' :: SourcePos -> [PVar BSort] -> BareType -> BareM SpecType
mkSpecType' l πs t
  = ofBRType env' t
  where
    env  = ofBareType_env l
    env' = env { te_resolveReft = te_resolveReft env . txParam subvUReft (uPVar <$> πs) t }

txParam f πs t = f (txPvar (predMap πs t))

txPvar :: M.HashMap Symbol UsedPVar -> UsedPVar -> UsedPVar 
txPvar m π = π { pargs = args' }
  where args' | not (null (pargs π)) = zipWith (\(_,x ,_) (t,_,y) -> (t, x, y)) (pargs π') (pargs π)
              | otherwise            = pargs π'
        π'    = fromMaybe (errorstar err) $ M.lookup (pname π) m
        err   = "Bare.replaceParams Unbound Predicate Variable: " ++ show π

predMap πs t = M.fromList [(pname π, π) | π <- πs ++ rtypePredBinds t]

rtypePredBinds = map uPVar . ty_preds . toRTypeRep

