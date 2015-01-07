{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Bare.Type.Transform (
    TransformEnv(..)
  , transformType
  ) where

import TyCon
import Var

import Control.Applicative
import Control.Monad.Reader

import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Misc (errorstar)

import Language.Haskell.Liquid.Misc (secondM)
import Language.Haskell.Liquid.RefType (symbolRTyVar)
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.Bare.Env (Out)

--------------------------------------------------------------------------------

type TransformEnv = TS { te_typeAliases :: M.HashMap Symbol (RTAlias RTyVar SpecType)
                       , te_lookupTyCon :: LocSymbol -> Out TyCon
                       , te_appRTAlias  :: RTAlias RTyVar SpecType -> {WTF} -> Out (RRType r)
                       }

type TransformM = ReaderT TransformEnv Out

transformType :: TransformEnv -> BRType r -> Out (RRType r)
transformType = flip runReaderT

--------------------------------------------------------------------------------

transformType' :: BRType r -> TransformM (RRType r)

transformType' (RApp c ts rs r)
  = do typeAliases <- asks te_typeAliases
       case M.lookup c typeAliases of
         Just rta ->
           do appRTAlias <- asks te_appRTAlias
              appRTAlias rta ts rs r
         Nothing ->
           do lookupTyCon <- asks te_lookupTyCon
              RApp <$> lookupTyCon c
                   <*> mapM transformType' ts
                   <*> mapM (mapM_RTProp transformType') rs
                   <*> pure r

transformType' (RVar a r)
  = return $ RVar (symbolRTyVar a) r
-- REVIEW: Behavior change - don't discard refts of RFun
transformType' (RFun x t1 t2 r)
  = RFun x <$> transformType' t1 <*> transformType' t2 <*> pure r
transformType' (RAllT a t)
  = RAllT (symbolRTyVar a) <$> transformType' t
transformType' (RAllP a t)
  = RAllP <$> mapM_pvar transformType' a <*> transformType' t
transformType' (RAllS x t)
  = RAllS x <$> transformType' t
transformType' (RAllE x t1 t2)
  = RAllE x <$> transformType' t1 <*> transformType' t2
transformType' (REx x t1 t2)
  = REx x <$> transformType' t1 <*> transformType' t2
transformType' (RExprArg e)
  = return $ RExprArg e
transformType' (RAppTy t1 t2 r)
  = RAppTy <$> transformType' t1 <*> transformType' t2 <*> pure r
transformType' (RRTy e r o t)
  = RRTy <$> mapM (secondM transformType') e <*> pure r <*> pure o <*> transformType' t
transformType' (ROth s)
  = return $ ROth s
transformType' (RHole r)
  = return $ RHole r

mapM_RTProp :: Monad m => (RType c tv r -> m (RType c' tv' r)) -> RTProp c tv r -> m (RTProp c' tv' r)
mapM_RTProp f (RPropP ss r)
  = RPropP <$> mapM (secondM f) ss <*> pure r
mapM_RTProp f (RProp ss t)
  = RProp <$> mapM (secondM f) ss <*> f t
mapM_RTProp f (RHProp _ _)
  = errorstar "TODO:EFFECTS:mapM_RTProp"

--------------------------------------------------------------------------------

bareTCApp r c rs ts | Just (SynonymTyCon rhs) <- synTyConRhs_maybe c
   = tyApp (subsTyVars_meet su $ ofType rhs) (drop nts ts) rs r 
   where tvs = tyConTyVars  c
         su  = zipWith (\a t -> (rTyVar a, toRSort t, t)) tvs ts
         nts = length tvs

-- TODO expandTypeSynonyms here to
bareTCApp r c rs ts | isFamilyTyCon c && isTrivial t
  = expandRTypeSynonyms $ t `strengthen` r 
  where t = rApp c ts rs mempty

bareTCApp r c rs ts 
  = rApp c ts rs r

tyApp (RApp c ts rs r) ts' rs' r' = RApp c (ts ++ ts') (rs ++ rs') (r `meet` r')
tyApp t                []  []  r  = t `strengthen` r
tyApp _                 _  _   _  = errorstar $ "Bare.Type.tyApp on invalid inputs"

expandRTypeSynonyms :: (PPrint r, Reftable r) => RRType r -> RRType r
expandRTypeSynonyms = ofType . expandTypeSynonyms . toType

