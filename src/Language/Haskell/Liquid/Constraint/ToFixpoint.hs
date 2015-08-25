module Language.Haskell.Liquid.Constraint.ToFixpoint (

        targetFInfo

        ) where

import qualified Language.Fixpoint.Types        as F
import Language.Haskell.Liquid.Constraint.Types

import Language.Haskell.Liquid.Types hiding     ( binds )
import Language.Fixpoint.Interface              ( parseFInfo )

import           Control.Applicative ((<$>))
import           Control.Arrow       ((***))
import qualified Data.HashMap.Strict            as M
import           Data.Monoid

import Language.Haskell.Liquid.Qualifier
import Language.Haskell.Liquid.GhcMisc          ( varSymbol )
import Language.Haskell.Liquid.RefType          ( rTypeSortedReft )

targetFInfo :: GhcInfo -> CGInfo -> F.FInfo Cinfo
targetFInfo info cgi
  = F.FI { F.cm       = M.fromList $ F.addIds $ fixCs cgi
         , F.ws       = fixWfs cgi
         , F.bs       = binds cgi
         , F.gs       = F.fromListSEnv . map mkSort $ M.toList $ meas spc
         , F.lits     = lits cgi
         , F.kuts     = kuts cgi
         , F.quals    = targetQuals info
         , F.bindInfo = (`Ci` Nothing) <$> bindSpans cgi
         }
   where
    spc    = spec info
    tce    = tcEmbeds spc
    mkSort = val *** (rTypeSortedReft tce . sort)

targetQuals :: GhcInfo -> [F.Qualifier]
targetQuals info = spcQs ++ genQs
  where
    spcQs     = M.elems $ qualifiers $ spec info
    genQs     = specificationQualifiers n info
    n         = maxParams $ config info

