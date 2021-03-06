{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.Liquid.Iface.Convert (
    -- * Iface Type Conversion
    toIface, fromIface
  ) where

import GHC hiding (L, Located)

import BinIface
import ConLike
import DataCon
import Exception
import FastMutInt
import Finder
import Fingerprint
import GhcMonad
import HscTypes
import Id
import IfaceEnv
import IfaceType hiding (IfaceType)
import IOEnv
import Kind
import Module
import Name
import NameEnv
import NameSet
import OccName
import PackageConfig
import Packages
import PrelNames
import TcIface
import TcRnDriver
import TcRnTypes
import TcRnMonad
import Unique
import UniqFM
import Var

import Control.Arrow
import Control.Monad

import Data.Data (Data)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Typeable (Typeable)
import Data.Version

import qualified Data.HashMap.Strict as M

import Text.Parsec.Pos

import Language.Fixpoint.Misc
import Language.Fixpoint.Types hiding (Found, Predicate)

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types hiding (L, R)

import Language.Haskell.Liquid.Iface.Binary ()
import Language.Haskell.Liquid.Iface.Ghc
import Language.Haskell.Liquid.Iface.Types

--------------------------------------------------------------------------------
-- Iface Type Conversion -------------------------------------------------------
--------------------------------------------------------------------------------

class Iface spec iface | spec -> iface where
  toIface   :: spec  -> iface 
  fromIface :: iface -> IfL spec

instance Iface (IfaceData GhcSpec) (IfaceData IfaceSpec) where
  toIface   = fmap toIface
  fromIface = traverse fromIface

-- TODO: Trim down what's included (add more export filtering)
instance Iface GhcSpec IfaceSpec where
  toIface (SP {..}) =
    IS { ifaceTySigs     = ofTySig <$> filter isExported (M.toList tySigs)
       , ifaceAsmSigs    = ofTySig <$> filter isExported (M.toList asmSigs)
       , ifaceCtors      = ofTySig <$> filter isExported (M.toList ctors)
       , ifaceMeas       = second toIface <$> M.toList meas
       , ifaceInvariants = fmap toIface <$> invariants
       , ifaceIAliases   = ofIAlias <$> ialiases
       , ifaceFreeSyms   = second getName <$> M.toList freeSyms
       , ifaceTcEmbeds   = first toIfaceTyCon <$> M.toList tcEmbeds
       , ifaceQualifiers = first getName <$> filter isExported (M.toList qualifiers)
       , ifaceTyConEnv   = ofTyConEnv <$> M.toList tyconEnv
       , ifaceRTEnv      = ofRTAlias <$> M.toList rtEnv
       , ifaceTInlines   = first getName <$> filter isExported (M.toList tinlines)
       , ifaceExports    = nameSetElems exports
       }
    where
      isExported (id, _) =
        isDataConWorkId id || elemNameSet (getName id) exports
      ofTySig    = getName      *** fmap toIface
      ofIAlias   = fmap toIface *** fmap toIface
      ofTyConEnv = toIfaceTyCon *** toIface
      ofRTAlias  = toIfaceTyCon *** toIface

  fromIface (IS {..}) = do
    tySigs     <- M.fromList <$> mapM ofTySig ifaceTySigs
    asmSigs    <- M.fromList <$> mapM ofTySig ifaceAsmSigs
    ctors      <- M.fromList <$> mapM ofTySig ifaceCtors
    meas       <- M.fromList <$> mapM (secondM fromIface) ifaceMeas
    invariants <- mapM (traverse fromIface) ifaceInvariants
    ialiases   <- mapM ofIAlias ifaceIAliases
    freeSyms   <- M.fromList <$> mapM (secondM lookupIfaceVar) ifaceFreeSyms
    tcEmbeds   <- M.fromList <$> mapM (firstM tcIfaceTyCon) ifaceTcEmbeds
    qualifiers <- M.fromList <$> mapM (firstM lookupIfaceVar) ifaceQualifiers
    tyconEnv   <- M.fromList <$> mapM ofTyConEnv ifaceTyConEnv
    rtEnv      <- M.fromList <$> mapM ofRTAlias ifaceRTEnv
    tinlines   <- M.fromList <$> mapM (firstM lookupIfaceVar) ifaceTInlines
    return $ mempty
      { tySigs     = tySigs
      , asmSigs    = asmSigs
      , ctors      = ctors
      , meas       = meas
      , invariants = invariants
      , ialiases   = ialiases
      , freeSyms   = freeSyms
      , tcEmbeds   = tcEmbeds
      , qualifiers = qualifiers
      , tyconEnv   = tyconEnv
      , rtEnv      = rtEnv
      , tinlines   = tinlines
      , exports    = mkNameSet ifaceExports
      }
    where
      ofTySig    (v, t) = (,) <$> lookupIfaceVar v     <*> traverse fromIface t
      ofIAlias   (x, y) = (,) <$> traverse fromIface x <*> traverse fromIface y
      ofTyConEnv (t, i) = (,) <$> tcIfaceTyCon t       <*> fromIface i
      ofRTAlias  (t, a) = (,) <$> tcIfaceTyCon t       <*> fromIface a

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
    bindIfaceTyVar (mkIfaceTvBndr tv) $ \tv' -> RAllT (rTyVar tv') <$> fromIface ty
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

instance Iface SpecMeasure IfaceMeasure where
  toIface M{..} =
    M (toIface sort) (map (getName *** toIface) defs)
  fromIface M{..} =
    M <$> fromIface sort <*> mapM ofDef defs
    where
      ofDef (dc, ty) = (,) <$> lookupIfaceVar dc <*> fromIface ty

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

instance Iface (RTAlias RTyVar SpecType) (RTAlias IfLclName IfaceType) where
  toIface (RTA ts es b) =
    RTA (toIface <$> ts) es (toIface <$> b)
  fromIface (RTA ts es b) =
    bindIfaceTyVars (map mkIfaceTvBndr ts) $ \ts' ->
      RTA (map rTyVar ts') es <$> traverse fromIface b

--------------------------------------------------------------------------------
-- Utiliy Functions ------------------------------------------------------------
--------------------------------------------------------------------------------

lookupIfaceVar :: Name -> IfL Var
lookupIfaceVar = fmap ofThing . tcIfaceGlobal
  where
    ofThing (AnId x) = x
    ofThing thing    = error $ "Bad result in lookupIfaceVar: " ++ showPpr thing

mkIfaceTvBndr :: IfLclName -> IfaceTvBndr
mkIfaceTvBndr tv = (tv, toIfaceKind liftedTypeKind)

