{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Haskell.Liquid.Spec.Env (
    SpecM
  , AnnInfo(..)

  , runSpecM

  , getWiredIns
  , mkFreshInt
  , getAllAnnInfo
  , lookupAnnInfo
  ) where

import GHC

import Annotations
import DynFlags
import Exception
import HscTypes
import Name
import Serialized

import Control.Applicative
import Control.Monad.State

import Data.List
import Data.Maybe
import Data.Monoid

import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Types

import qualified Language.Haskell.Liquid.RType as RT

import Language.Haskell.Liquid.GhcMisc

import Language.Haskell.Liquid.Spec.WiredIns

--------------------------------------------------------------------------------

newtype SpecM a = SpecM { unSpecM :: StateT SpecState Ghc a }
                  deriving ( Functor, Applicative, Monad
                           , MonadIO
                           )

data SpecState = SS { ss_wiredIns :: WiredIns
                    , ss_annotEnv :: M.HashMap Name AnnInfo
                    , ss_freshInt :: Integer
                    }

data AnnInfo = AI { ai_exprParams :: [Symbol]
                  , ai_ftcEmbed   :: Maybe FTycon
                  , ai_isInline   :: Bool
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


instance Monoid AnnInfo where
  mempty =
    AI [] Nothing False
  mappend x y =
    AI (ai_exprParams x ++ ai_exprParams y)
       (ai_ftcEmbed x <|> ai_ftcEmbed y)
       (ai_isInline x || ai_isInline y)
  mconcat xs =
    AI (concatMap ai_exprParams xs)
       (listToMaybe $ mapMaybe ai_ftcEmbed xs)
       (or $ map ai_isInline xs)

--------------------------------------------------------------------------------

runSpecM :: SpecM a -> ModGuts -> Ghc a
runSpecM act guts = do
  wis <- loadWiredIns
  ann <- buildAnnotEnv guts
  runSpecM' act $ SS wis ann 0
  
runSpecM' :: SpecM a -> SpecState -> Ghc a
runSpecM' = evalStateT . unSpecM


-- TODO: Error out on duplicate annotations
-- TODO: Move this code elsewhere?
buildAnnotEnv :: ModGuts -> Ghc (M.HashMap Name AnnInfo)
buildAnnotEnv guts =
  return $ M.fromListWith mappend $ mapMaybe go $ mg_anns guts
  where
    go (Annotation (NamedTarget name) payload)
      | Just (RT.ExprParams params) <- fromSerialized deserializeWithData payload =
        Just (name, mempty { ai_exprParams = map symbol params })
      | Just (RT.EmbedAs ftc) <- fromSerialized deserializeWithData payload =
        Just (name, mempty { ai_ftcEmbed = Just $ convertFTycon ftc })
      | Just RT.IsInline <- fromSerialized deserializeWithData payload =
        Just (name, mempty { ai_isInline = True })
    go _ = Nothing

convertFTycon :: RT.FTycon -> FTycon
convertFTycon RT.FTcInt      = intFTyCon
convertFTycon RT.FTcReal     = realFTyCon
convertFTycon RT.FTcBool     = boolFTyCon
convertFTycon (RT.FTcUser s) = symbolFTycon (dummyLoc $ symbol s) -- TODO: Preserve location info

--------------------------------------------------------------------------------

getWiredIns :: SpecM WiredIns
getWiredIns = SpecM $ gets ss_wiredIns

mkFreshInt :: SpecM Integer
mkFreshInt = SpecM $ do
  state@(SS { ss_freshInt = freshInt }) <- get
  put $ state { ss_freshInt = freshInt + 1 }
  return freshInt

getAllAnnInfo :: SpecM [(Name, AnnInfo)]
getAllAnnInfo = SpecM $ gets (M.toList . ss_annotEnv)

lookupAnnInfo :: NamedThing a => a -> SpecM AnnInfo
lookupAnnInfo thing = SpecM $ do
  ann <- gets ss_annotEnv
  return $ M.lookupDefault mempty (getName thing) ann

