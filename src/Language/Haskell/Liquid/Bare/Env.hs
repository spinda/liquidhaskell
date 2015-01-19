module Language.Haskell.Liquid.Bare.Env (
    BareM
  , Warn
  , TCEnv

  , BareEnv(..)

  , inModule

  , setRTAlias
  , setRPAlias
  , setREAlias

  , execBare
  ) where

import HscTypes
import TyCon
import Var

import Control.Monad.Error hiding (Error)
import Control.Monad.State
import Control.Monad.Writer

import qualified Control.Exception   as Ex 
import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Types (Symbol)

import Language.Haskell.Liquid.Errors ()
import Language.Haskell.Liquid.Types

-----------------------------------------------------------------------------------
-- | Error-Reader-IO For Bare Transformation --------------------------------------
-----------------------------------------------------------------------------------

-- FIXME: don't use WriterT [], very slow
type BareM = WriterT [Warn] (ErrorT Error (StateT BareEnv IO))

type Warn  = String

type TCEnv = M.HashMap TyCon RTyCon

data BareEnv = BE { modName  :: !ModName
                  , tcEnv    :: !TCEnv
                  , rtEnv    :: !RTEnv
                  , varEnv   :: ![(Symbol,Var)]
                  , hscEnv   :: HscEnv 
                  , logicEnv :: LogicMap
                  }

setModule m b = b { modName = m }

inModule m act = do
  old <- gets modName
  modify $ setModule m
  res <- act
  modify $ setModule old
  return res


setRTAlias s a =
  modify $ \b -> b { rtEnv = mapRT (M.insert s a) $ rtEnv b }

setRPAlias s a =
  modify $ \b -> b { rtEnv = mapRP (M.insert s a) $ rtEnv b }

setREAlias s a =
  modify $ \b -> b { rtEnv = mapRE (M.insert s a) $ rtEnv b }

------------------------------------------------------------------
execBare :: BareM a -> BareEnv -> IO (Either Error a)
------------------------------------------------------------------
execBare act benv = 
   do z <- evalStateT (runErrorT (runWriterT act)) benv `Ex.catch` (return . Left)
      case z of
        Left s        -> return $ Left s
        Right (x, ws) -> do forM_ ws $ putStrLn . ("WARNING: " ++) 
                            return $ Right x

