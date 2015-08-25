{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Iface.Types (
    IfaceData(..)
  , IfaceSpec(..)

  , IRType
  , ISort
  , IPVar
  , IfaceType
  , IfaceMeasure
  , ITyCon(..)

  , emptyIfaceData
  , mkIfaceFingerprint
  ) where

import Fingerprint
import IfaceType hiding (IfaceType)
import Module
import Name
import Name
import TyCon

import Control.Arrow

import Data.List

import Text.Printf

import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Types

--------------------------------------------------------------------------------
-- Types for LiquidHaskell Interface Files -------------------------------------
--------------------------------------------------------------------------------

data IfaceData spec = ID { ifaceModule       :: !Module
                         , ifaceFingerprint  :: !Fingerprint
                         , ifaceSpec         :: !spec
                         } deriving (Foldable, Functor, Traversable)

data IfaceSpec = IS { ifaceTySigs     :: ![(Name, Located IfaceType)]
                    , ifaceAsmSigs    :: ![(Name, Located IfaceType)]
                    , ifaceCtors      :: ![(Name, Located IfaceType)]
                    , ifaceMeas       :: ![(LocSymbol, IfaceMeasure)]
                    , ifaceInvariants :: ![Located IfaceType]
                    , ifaceIAliases   :: ![(Located IfaceType, Located IfaceType)]
                    , ifaceFreeSyms   :: ![(Symbol, Name)]
                    , ifaceTcEmbeds   :: ![(IfaceTyCon, FTycon)]
                    , ifaceQualifiers :: ![(Name, Qualifier)]
                    , ifaceTyConEnv   :: ![(IfaceTyCon, ITyCon)]
                    , ifaceRTEnv      :: ![(IfaceTyCon, RTAlias IfLclName IfaceType)]
                    , ifaceTInlines   :: ![(Name, TInline)]
                    , ifaceExports    :: ![Name]
                    }


type IRType       = RType ITyCon IfLclName
type ISort        = IRType ()
type IPVar        = PVar ISort
type IfaceType    = IRType RReft
type IfaceMeasure = Measure IfaceType Name

data ITyCon = ITyCon
  { itc_tc    :: IfaceTyCon    -- ^ GHC Type Constructor
  , itc_pvars :: ![IPVar]      -- ^ Predicate Parameters
  , itc_info  :: !TyConInfo    -- ^ TyConInfo
  }


emptyIfaceData :: Monoid spec => Module -> IfaceData spec
emptyIfaceData mod = ID mod fingerprint0 mempty

mkIfaceFingerprint :: Module -> FilePath -> [IfaceData spec] -> IO Fingerprint
mkIfaceFingerprint mod sourceFile externData = do
  sourceHash <- getFileHash sourceFile
  let digests = map mkDigest $ (mod, sourceHash) : externFingerprints
  let summary = intercalate ";" digests
  return $ fingerprintString summary 
  where
    externFingerprints   = sortBy cmp $ map (ifaceModule &&& ifaceFingerprint) externData
    cmp (x, _) (y, _)    = stableModuleCmp x y
    mkDigest (mod, hash) =
      printf "%s:%s=%s" (packageKeyString $ modulePackageKey mod)
                        (moduleNameString $ moduleName mod)
                        (show hash)

