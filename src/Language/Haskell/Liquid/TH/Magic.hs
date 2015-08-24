-- | Temporary workarounds for limitation in Template Haskell as implemented
-- in GHC 7.10.

module Language.Haskell.Liquid.TH.Magic (
    -- * Workarounds to TH Limitations
    forceAddAnnotation
  ) where

import Convert
import HsDecls
import Name
import Outputable
import RdrName
import RnEnv
import SrcLoc
import TcRnMonad
import TcRnTypes
import TcSplice
import Unique

import Data.Maybe

import qualified Language.Haskell.TH.Syntax as TH

import Unsafe.Coerce

--------------------------------------------------------------------------------
-- Break Into TcRn From Q ------------------------------------------------------
--------------------------------------------------------------------------------

tcRnQ :: TcRn a -> TH.Q a
tcRnQ m = TH.Q $ unsafeCoerce m

tcRnFail :: String -> TcRn a
tcRnFail s = TH.qReport True s >> fail "Q monad failure"

--------------------------------------------------------------------------------
-- Workarounds to TH Limitations -----------------------------------------------
--------------------------------------------------------------------------------

-- | Template Haskell under GHC 7.10 doesn't let addTopDecls add annotations,
-- and doesn't like adding annotations to local variables. This function goes
-- around these restrictions until we can get them lifted in GHC.
forceAddAnnotation :: TH.AnnTarget -> TH.Exp -> TH.Q ()
forceAddAnnotation tgt exp = tcRnQ $ do
  loc <- getSrcSpanM
  [L l (AnnD (HsAnnotation src tgt' exp'))] <-
    case convertToHsDecls loc [TH.PragmaD $ TH.AnnP tgt exp] of
      Left  e ->
        pprPanic "forceAddAnnotation: can't convert annotation declaration" e
      Right cvt ->
        return cvt
  tgt'' <- resolveAnnProvenance tgt'
  let decl = L l $ AnnD $ HsAnnotation src tgt'' exp'
  topDecls <- tcg_th_topdecls <$> getGblEnv
  updTcRef topDecls (\tds -> decl : tds)

resolveAnnProvenance :: AnnProvenance RdrName -> TcRn (AnnProvenance RdrName)
resolveAnnProvenance (ValueAnnProvenance name) = ValueAnnProvenance <$> resolveRdrName name
resolveAnnProvenance (TypeAnnProvenance  name) = TypeAnnProvenance  <$> resolveRdrName name
resolveAnnProvenance ModuleAnnProvenance       = return ModuleAnnProvenance

resolveRdrName :: Located RdrName -> TcRn (Located RdrName)
resolveRdrName rdr = do
  L l name <- lookupLocatedOccRn rdr
  L l . Exact <$> resolveName name

resolveName :: Name -> TcRn Name
resolveName name
  | isExternalName name = return name
  | otherwise = do
    mod <- tcg_mod <$> getGblEnv
    return $ mkExternalName (getUnique name) mod (nameOccName name) (getSrcSpan name)

