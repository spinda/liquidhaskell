{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Liquid.Spec.Env (
    SpecM
  , runSpecM

  , getWiredIns
  , mkFreshInt
  ) where

import GHC

import DynFlags
import Exception

import Control.Monad.State

import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------

newtype SpecM a = SpecM { unSpecM :: StateT SpecState Ghc a }
                  deriving ( Functor, Applicative, Monad
                           , MonadIO
                           )

data SpecState = SS { ss_wiredIns :: WiredIns
                    , ss_freshInt :: Integer
                    }

instance GhcMonad SpecM where
  getSession = SpecM $ lift getSession
  setSession = SpecM . lift . setSession

instance ExceptionMonad SpecM where
  gcatch act handle = SpecM $ do
    state <- get
    lift $ runSpecM' act state
             `gcatch` \e -> runSpecM' (handle e) state
  gmask f = SpecM $ do
    state <- get
    lift $ gmask $ \ghc_restore ->
      runSpecM' (f $ wired_restore ghc_restore) state
    where
      wired_restore ghc_restore act = SpecM $ do
         state <- get
         lift $ ghc_restore $ runSpecM' act state

instance HasDynFlags SpecM where
  getDynFlags = SpecM $ lift getDynFlags

--------------------------------------------------------------------------------

runSpecM :: SpecM a -> Ghc a
runSpecM act = do
  wis <- loadWiredIns
  let initState = SS wis 0
  runSpecM' act initState
  
runSpecM' :: SpecM a -> SpecState -> Ghc a
runSpecM' = evalStateT . unSpecM

--------------------------------------------------------------------------------

getWiredIns :: SpecM WiredIns
getWiredIns = SpecM $ gets ss_wiredIns

mkFreshInt :: SpecM Integer
mkFreshInt = SpecM $ do
  state@(SS { ss_freshInt = freshInt }) <- get
  put $ state { ss_freshInt = freshInt + 1 }
  return freshInt

