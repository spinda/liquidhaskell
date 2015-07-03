{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.Liquid.Iface (
    writeIfaceSpec
  , readIfaceSpec
  ) where

import GHC hiding (L, Located)

import BinIface
import Exception
import FastMutInt
import GhcMonad
import HscTypes
import IfaceEnv
import IfaceType hiding (IfaceType)
import IOEnv
import Kind
import NameSet
import OccName
import TcIface
import TcRnDriver
import TcRnTypes
import TcRnMonad
import UniqFM
import Var

import Control.Arrow

import Data.Data (Data)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Typeable (Typeable)

import qualified Data.HashMap.Strict as M

import Text.Parsec.Pos

import Language.Fixpoint.Misc
import Language.Fixpoint.Types hiding (Predicate)

import Language.Haskell.Liquid.CmdLine
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types hiding (L, R)

import Language.Haskell.Liquid.Iface.Binary ()
import Language.Haskell.Liquid.Iface.Ghc
import Language.Haskell.Liquid.Iface.Types

--------------------------------------------------------------------------------
-- Read/Write Liquid Interface Files -------------------------------------------
--------------------------------------------------------------------------------

writeIfaceSpec :: FilePath -> GhcSpec -> IO ()
writeIfaceSpec path = writeIfaceFile path . toIface 

readIfaceSpec :: GhcMonad m => FilePath -> Module -> m GhcSpec
readIfaceSpec path mod = do
  spec <- readIfaceFile path
  runTcM $ initIfaceTc (emptyModIface mod) $ \_ -> fromIface spec

--------------------------------------------------------------------------------
-- Iface Type Conversion -------------------------------------------------------
--------------------------------------------------------------------------------

class Iface spec iface | spec -> iface where
  toIface   :: spec  -> iface 
  fromIface :: iface -> IfL spec

instance Iface GhcSpec IfaceSpec where
  toIface (SP {..}) =
    IS { ifaceTySigs     = ofTySig <$> filter isExported tySigs
       , ifaceAsmSigs    = ofTySig <$> filter isExported asmSigs
       , ifaceCtors      = ofTySig <$> filter isExported ctors
       , ifaceMeas       = second (fmap toIface) <$> meas
       , ifaceInvariants = fmap toIface <$> invariants
       , ifaceIAliases   = ofIAlias <$> ialiases
       , ifaceFreeSyms   = second getOccName <$> freeSyms
       , ifaceTcEmbeds   = first toIfaceTyCon <$> M.toList tcEmbeds
       , ifaceQualifiers = qualifiers
       , ifaceTyConEnv   = ofTyConEnv <$> M.toList tyconEnv
       , ifaceExports    = nameSetElems exports
       }
    where
      isExported = (`elemNameSet` exports) . getName . fst
      ofTySig    = getOccName   *** fmap toIface
      ofIAlias   = fmap toIface *** fmap toIface
      ofTyConEnv = toIfaceTyCon *** toIface

  fromIface (IS {..}) = do
    tySigs     <- mapM ofTySig ifaceTySigs
    asmSigs    <- mapM ofTySig ifaceAsmSigs
    ctors      <- mapM ofTySig ifaceCtors
    meas       <- mapM (secondM (traverse fromIface)) ifaceMeas
    invariants <- mapM (traverse fromIface) ifaceInvariants
    ialiases   <- mapM ofIAlias ifaceIAliases
    freeSyms   <- mapM (secondM lookupIfaceVar) ifaceFreeSyms
    tcEmbeds   <- M.fromList <$> mapM (firstM tcIfaceTyCon) ifaceTcEmbeds
    tyconEnv   <- M.fromList <$> mapM ofTyConEnv ifaceTyConEnv
    return $ (emptySpec mempty)
      { tySigs     = tySigs
      , asmSigs    = asmSigs
      , ctors      = ctors
      , meas       = meas
      , invariants = invariants
      , ialiases   = ialiases
      , freeSyms   = freeSyms
      , tcEmbeds   = tcEmbeds
      , qualifiers = ifaceQualifiers
      , tyconEnv   = tyconEnv
      , exports    = mkNameSet ifaceExports
      }
    where
      ofTySig    (v, t) = (,) <$> lookupIfaceVar v     <*> traverse fromIface t
      ofIAlias   (x, y) = (,) <$> traverse fromIface x <*> traverse fromIface y
      ofTyConEnv (t, i) = (,) <$> tcIfaceTyCon t       <*> fromIface i

instance Iface (RRType r) (IRType r) where
  toIface (RVar tv r)       = RVar (toIface tv) r
  toIface (RFun b i o r)    = RFun b (toIface i) (toIface o) r
  toIface (RAllT tv ty)     = RAllT (toIface tv) (toIface ty)
  toIface (RAllP pv ty)     = RAllP (toIface pv) (toIface ty)
  toIface (RAllS sb ty)     = RAllS sb (toIface ty)
  toIface (RApp tc as ps r) = RApp (toIface tc) (toIface <$> as) (toIface <$> ps) r
  toIface (RAllE b a ty)    = RAllE b (toIface a) (toIface ty)
  toIface (REx b e ty)      = REx b (toIface e) (toIface ty)
  toIface (RExprArg e)      = RExprArg e
  toIface (RAppTy t1 t2 r)  = RAppTy (toIface t1) (toIface t2) r
  toIface (RRTy env r o ty) = RRTy (second toIface <$> env) r o (toIface ty)
  toIface (RHole r)         = RHole r

  fromIface (RVar tv r) =
    (`RVar` r) <$> fromIface tv
  fromIface (RFun b i o r) =
    RFun b <$> fromIface i <*> fromIface o <*> pure r
  fromIface (RAllT tv ty) =
    bindIfaceTyVar (tv, toIfaceKind liftedTypeKind) $ \tv' -> RAllT (rTyVar tv') <$> fromIface ty
  fromIface (RAllP pv ty) =
    RAllP <$> fromIface pv <*> fromIface ty
  fromIface (RAllS sb ty) =
    RAllS sb <$> fromIface ty
  fromIface (RApp tc as ps r) =
    RApp <$> fromIface tc <*> mapM fromIface as <*> mapM fromIface ps <*> pure r
  fromIface (RAllE b a ty) =
    RAllE b <$> fromIface a <*> fromIface ty
  fromIface (REx b e ty) =
    REx b <$> fromIface e <*> fromIface ty
  fromIface (RExprArg e) =
    return $ RExprArg e
  fromIface (RAppTy t1 t2 r) =
    RAppTy <$> fromIface t1 <*> fromIface t2 <*> pure r
  fromIface (RRTy env r o ty) =
    RRTy <$> mapM (secondM fromIface) env <*> pure r <*> pure o <*> fromIface ty
  fromIface (RHole r) =
    return $ RHole r

instance Iface RTyCon ITyCon where
  toIface rtc =
    ITyCon (toIfaceTyCon $ rtc_tc rtc)
           (toIface <$> rTyConPVs rtc)
           (rtc_info rtc)
  fromIface itc =
    RTyCon <$> tcIfaceTyCon (itc_tc itc)
           <*> mapM fromIface (itc_pvars itc)
           <*> pure (itc_info itc)

instance Iface RTyVar IfLclName where
  toIface   (RTV tv) = toIfaceTyVar tv
  fromIface tv       = rTyVar <$> tcIfaceTyVar tv

instance Iface s i => Iface (PVar s) (PVar i) where
  toIface   (PV {..}) = PV pname (toIface ptype) parg (mapFst3 toIface <$> pargs)
  fromIface (PV {..}) = PV pname <$> fromIface ptype <*> pure parg <*> mapM (first3M fromIface) pargs

instance Iface s i => Iface (PVKind s) (PVKind i) where
  toIface (PVProp t) = PVProp (toIface t)
  toIface PVHProp    = PVHProp

  fromIface (PVProp t) = PVProp <$> fromIface t
  fromIface PVHProp    = return PVHProp

instance (Iface τs τi, Iface ts ti) =>
         Iface (Ref τs r ts) (Ref τi r ti) where
  toIface (RPropP as r) = RPropP (second toIface <$> as) r
  toIface (RProp  as b) = RProp  (second toIface <$> as) (toIface b)
  toIface (RHProp as h) = RHProp (second toIface <$> as) (toIface h)

  fromIface (RPropP as r) = (`RPropP` r) <$> (mapM (secondM fromIface) as)
  fromIface (RProp  as b) = RProp        <$> (mapM (secondM fromIface) as) <*> fromIface b
  fromIface (RHProp as h) = RHProp       <$> (mapM (secondM fromIface) as) <*> fromIface h

instance Iface s i => Iface (World s) (World i) where
  toIface   (World hs) = World (toIface <$> hs)
  fromIface (World hs) = World <$> mapM fromIface hs

instance Iface s i => Iface (HSeg s) (HSeg i) where
  toIface (HBind a v) = HBind a (toIface v)
  toIface (HVar pv)   = HVar pv

  fromIface (HBind a v) = HBind a <$> fromIface v
  fromIface (HVar pv)   = return $ HVar pv

--------------------------------------------------------------------------------
-- Utiliy Functions ------------------------------------------------------------
--------------------------------------------------------------------------------

lookupIfaceVar :: OccName -> IfL Var
lookupIfaceVar v = do
  hscEnv    <- env_top <$> getEnv
  name      <- lookupIfaceTop v
  (msgs, m) <- liftIO $ tcRnLookupName hscEnv name
  case m of
    Nothing    -> liftIO $ throwIO $ mkSrcErr $ snd msgs
    Just thing -> return $ tyThingId thing

