{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Haskell.Liquid.Iface.Types (
    IfaceData(..)
  , emptyIfaceData
  , IfaceSpec(..)
  , IRType
  , ISort
  , IPVar
  , IfaceType
  , ITyCon(..)
  ) where

import Fingerprint
import IfaceType hiding (IfaceType)
import Module
import Name
import Name
import TyCon

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Types

--------------------------------------------------------------------------------
-- Types for LiquidHaskell Interface Files -------------------------------------
--------------------------------------------------------------------------------

data IfaceData spec = ID { ifaceModule       :: !Module
                         , ifaceFingerprint  :: !Fingerprint
                         , ifaceDependencies :: ![(Module, Fingerprint)]
                         , ifaceSpec         :: !spec
                         } deriving (Foldable, Functor, Traversable)

emptyIfaceData :: Monoid spec => Module -> IfaceData spec
emptyIfaceData mod = ID mod fingerprint0 [] mempty

data IfaceSpec = IS { ifaceTySigs     :: ![(Name, Located IfaceType)]
                    , ifaceAsmSigs    :: ![(Name, Located IfaceType)]
                    , ifaceCtors      :: ![(Name, Located IfaceType)]
                    , ifaceMeas       :: ![(Symbol, Located IfaceType)]
                    , ifaceInvariants :: ![Located IfaceType]
                    , ifaceIAliases   :: ![(Located IfaceType, Located IfaceType)]
                    , ifaceFreeSyms   :: ![(Symbol, Name)]
                    , ifaceTcEmbeds   :: ![(IfaceTyCon, FTycon)]
                    , ifaceQualifiers :: ![Qualifier]
                    , ifaceTyConEnv   :: ![(IfaceTyCon, ITyCon)]
                    , ifaceRTEnv      :: ![(IfaceTyCon, RTAlias IfLclName IfaceType)]
                    , ifaceTInlines   :: ![(Name, TInline)]
                    , ifaceExports    :: ![Name]
                    }

type IRType    = RType ITyCon IfLclName
type ISort     = IRType ()
type IPVar     = PVar ISort
type IfaceType = IRType RReft

data ITyCon = ITyCon
  { itc_tc    :: IfaceTyCon    -- ^ GHC Type Constructor
  , itc_pvars :: ![IPVar]      -- ^ Predicate Parameters
  , itc_info  :: !TyConInfo    -- ^ TyConInfo
  }

