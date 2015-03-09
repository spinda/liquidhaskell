module Language.Haskell.Liquid.Bare.Misc (
    MapTyVarST(..)
  , initMapSt
  , runMapTyVars
  , mapTyVars

  , symbolRTyVar
  ) where

import Type
import TypeRep

import Control.Applicative ((<$>))
import Control.Monad.Error (throwError)
import Control.Monad.State

import Language.Fixpoint.Types (Reftable(..))

import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

-- TODO: This is where unsorted stuff is for now. Find proper places for what follows.

-------------------------------------------------------------------------------
-- Renaming Type Variables in Haskell Signatures ------------------------------
-------------------------------------------------------------------------------

data MapTyVarST = MTVST { vmap   :: [(Var, RTyVar)]
                        , errmsg :: Error 
                        }

initMapSt = MTVST []

-- TODO: Maybe don't expose this; instead, roll this in with mapTyVar and export a
--       single "clean" function as the API.
runMapTyVars :: StateT MapTyVarST (Either Error) () -> MapTyVarST -> Either Error MapTyVarST
runMapTyVars x s = execStateT x s

mapTyVars :: (PPrint r, Reftable r) => Type -> RRType r -> StateT MapTyVarST (Either Error) ()
mapTyVars τ (RAllT _ t)   
  = mapTyVars τ t
mapTyVars (ForAllTy _ τ) t 
  = mapTyVars τ t
mapTyVars (FunTy τ τ') (RFun _ t t' _) 
   = mapTyVars τ t  >> mapTyVars τ' t'
mapTyVars (TyConApp _ τs) (RApp _ ts _ _) 
   = zipWithM_ mapTyVars τs ts
mapTyVars (TyVarTy α) (RVar a _)      
   = do s  <- get
        s' <- mapTyRVar α a s
        put s'
mapTyVars τ (RAllP _ t)   
  = mapTyVars τ t 
mapTyVars τ (RAllS _ t)   
  = mapTyVars τ t 
mapTyVars τ (RAllE _ _ t)   
  = mapTyVars τ t 
mapTyVars τ (RRTy _ _ _ t)
  = mapTyVars τ t
mapTyVars τ (REx _ _ t)
  = mapTyVars τ t 
mapTyVars _ (RExprArg _)
  = return ()
mapTyVars (AppTy τ τ') (RAppTy t t' _) 
  = do  mapTyVars τ t 
        mapTyVars τ' t' 
mapTyVars _ (RHole _)
  = return ()
mapTyVars _ _
  = throwError =<< errmsg <$> get

mapTyRVar α a s@(MTVST αas err)
  = case lookup α αas of
      Just a' | a == a'   -> return s
              | otherwise -> throwError err
      Nothing             -> return $ MTVST ((α,a):αas) err

