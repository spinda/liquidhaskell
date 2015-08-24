{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Haskell.Liquid.Pipeline.Rewrite (
    rewriteModule
  ) where

import GHC

import Bag
import FastString
import HscTypes
import HsDecls
import HsExpr
import HsSyn
import Module
import OccName
import RdrName

import Data.List

import Language.Haskell.Liquid.GhcMisc

import Language.Haskell.Liquid.TH.WiredIns

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

rewriteModule :: ParsedModule -> ParsedModule
rewriteModule = rewrite

--------------------------------------------------------------------------------
-- Rewrite Visitors ------------------------------------------------------------
--------------------------------------------------------------------------------

class Rewrite a where
  rewrite :: a -> a


instance Rewrite a => Rewrite (Located a) where
  rewrite = fmap rewrite

instance Rewrite a => Rewrite [a] where
  rewrite = map rewrite

instance Rewrite a => Rewrite (Bag a) where
  rewrite = mapBag rewrite


instance Rewrite ParsedModule where
  rewrite pm@ParsedModule{..} =
    pm { pm_mod_summary   = rewrite pm_mod_summary
       , pm_parsed_source = rewrite pm_parsed_source
       }

instance Rewrite ModSummary where
  rewrite ms@ModSummary{..} =
    ms { ms_textual_imps = rewrite ms_textual_imps
       }

-- TODO: Perform a lookup to make sure this is the exact module we're
--       targeting, and not the (very rare) edge case of another module
--       named LiquidHaskell.
instance Rewrite (ImportDecl RdrName) where
  rewrite im@ImportDecl{..} =
    im { ideclName = rewrite ideclName
       }

instance Rewrite ModuleName where
  rewrite mn
    | mn == mkModuleName "LiquidHaskell" = mkModuleName "LiquidHaskell_"
    | otherwise                          = mn

instance Rewrite (HsModule RdrName) where
  rewrite hm@HsModule{..} =
    hm { hsmodImports = rewrite hsmodImports
       , hsmodDecls   = rewrite hsmodDecls
       }

instance Rewrite (HsDecl RdrName) where
  rewrite (TyClD d) = TyClD $ rewrite d
  rewrite (InstD d) = InstD $ rewrite d
  rewrite (ValD  d) = ValD  $ rewrite d
  rewrite d         = d

instance Rewrite (TyClDecl RdrName) where
  rewrite dd@DataDecl{..} =
    dd { tcdDataDefn = rewrite tcdDataDefn
       }
  rewrite cd@ClassDecl{..} =
    cd { tcdSigs = rewrite tcdSigs
       }
  rewrite d = d

instance Rewrite (HsDataDefn RdrName) where
  rewrite dd@HsDataDefn{..} =
    dd { dd_cons = rewrite dd_cons
       }

instance Rewrite (ConDecl RdrName) where
  rewrite cd@ConDecl{..} =
    cd { con_details = rewrite con_details
       , con_res     = rewriteGADT (unLoc <$> con_names) con_res
       }

instance Rewrite (HsConDeclDetails RdrName) where
  rewrite (RecCon rec) = RecCon $ rewrite rec
  rewrite d            = d

instance Rewrite (ConDeclField RdrName) where
  rewrite cf@ConDeclField{..} =
    cf { cd_fld_type = rewriteType (map unLoc cd_fld_names) <$> cd_fld_type
       }

instance Rewrite (InstDecl RdrName) where
  rewrite (ClsInstD d) = ClsInstD $ rewrite d
  rewrite d            = d

instance Rewrite (ClsInstDecl RdrName) where
  rewrite cd@ClsInstDecl{..} =
    cd { cid_binds = rewrite cid_binds
       , cid_sigs  = rewrite cid_sigs
       }

instance Rewrite (HsBind RdrName) where
  rewrite fb@FunBind{..} =
    fb { fun_matches = rewrite fun_matches
       }
  rewrite b = b

instance Rewrite (MatchGroup RdrName (LHsExpr RdrName)) where
  rewrite mg@MG{..} =
    mg { mg_alts = rewrite mg_alts
       }

instance Rewrite (Match RdrName (LHsExpr RdrName)) where
  rewrite mh@Match{..} =
    mh { m_grhss = rewrite m_grhss
       }

instance Rewrite (GRHSs RdrName (LHsExpr RdrName)) where
  rewrite gr@GRHSs{..} =
    gr { grhssLocalBinds = rewrite grhssLocalBinds
       }

instance Rewrite (HsLocalBinds RdrName) where
  rewrite (HsValBinds vb) = HsValBinds $ rewrite vb
  rewrite b               = b

instance Rewrite (HsValBinds RdrName) where
  rewrite (ValBindsIn bs ss) = ValBindsIn (rewrite bs) (rewrite ss)
  rewrite b                  = b

instance Rewrite (Sig RdrName) where
  rewrite (TypeSig ns ty pr) = TypeSig ns (rewriteType (map unLoc ns) <$> ty) pr
  rewrite s                  = s


rewriteGADT :: [RdrName] -> ResType (LHsType RdrName) -> ResType (LHsType RdrName)
rewriteGADT ns (ResTyGADT ss ty) = ResTyGADT ss $ rewriteType ns <$> ty
rewriteGADT _ r                  = r

-- TODO: Perform a lookup to make sure this is the exact lq quasiquoter we're
--       targeting
rewriteType :: [RdrName] -> HsType RdrName -> HsType RdrName
rewriteType ns (HsQuasiQuoteTy qq) =
  HsQuasiQuoteTy $ rewriteQuasiQuote ns qq
rewriteType ns (HsForAllTy Implicit s b c ty) =
  HsForAllTy Implicit s b c $ rewriteType ns <$> ty
rewriteType _  ty = ty

rewriteQuasiQuote :: [RdrName] -> HsQuasiQuote RdrName -> HsQuasiQuote RdrName
rewriteQuasiQuote ns (HsQuasiQuote id sp fs)
  | occNameString (rdrNameOcc id) == "lq" =
    HsQuasiQuote id sp $
      appendFS (fsLit lqTypeParsePrefix) $
        appendFS (fsLit $ intercalate "," $ map showPpr ns) $
          consFS '|' fs
rewriteQuasiQuote _ qq = qq

