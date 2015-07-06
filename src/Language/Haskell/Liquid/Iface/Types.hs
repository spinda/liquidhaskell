module Language.Haskell.Liquid.Iface.Types (
    IfaceSpec(..)
  , IRType
  , ISort
  , IPVar
  , IfaceType
  , ITyCon(..)
  ) where

import IfaceType hiding (IfaceType)
import Name
import OccName
import TyCon

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Types

--------------------------------------------------------------------------------
-- Types for LiquidHaskell Interface Files -------------------------------------
--------------------------------------------------------------------------------

data IfaceSpec = IS { ifaceTySigs     :: ![(OccName, Located IfaceType)]
                    , ifaceAsmSigs    :: ![(OccName, Located IfaceType)]
                    , ifaceCtors      :: ![(OccName, Located IfaceType)]
                    , ifaceMeas       :: ![(Symbol, Located IfaceType)]
                    , ifaceInvariants :: ![Located IfaceType]
                    , ifaceIAliases   :: ![(Located IfaceType, Located IfaceType)]
                    , ifaceFreeSyms   :: ![(Symbol, OccName)]
                    , ifaceTcEmbeds   :: ![(IfaceTyCon, FTycon)]
                    , ifaceQualifiers :: ![Qualifier]
                    , ifaceTyConEnv   :: ![(IfaceTyCon, ITyCon)]
                    , ifaceRTEnv      :: ![(IfaceTyCon, RTAlias IfLclName IfaceType)]
                    , ifaceTInlines   :: ![(OccName, TInline)]
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

