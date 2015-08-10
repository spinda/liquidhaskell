{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}

module Language.Haskell.Liquid.Measure (
    dataConTypes
  , defRefType
  , strLen
  , wiredInMeasures
  ) where

import GHC hiding (Located)
import Var
import Type
import TysPrim
import TysWiredIn
import Text.PrettyPrint.HughesPJ
import Text.Printf (printf)
import DataCon

import qualified Data.HashMap.Strict as M 
import qualified Data.HashSet        as S 
import Data.List (foldl')

import Data.Monoid hiding ((<>))
import Control.Applicative      ((<$>))

import Data.Maybe (fromMaybe)

import Language.Fixpoint.Misc
import Language.Fixpoint.Types hiding (Def, R)
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Types    hiding (GhcInfo(..), GhcSpec (..))
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Variance
import Language.Haskell.Liquid.Bounds


-- MOVE TO TYPES
instance Functor CMeasure where
  fmap f (CM n t) = CM n (f t)

-- MOVE TO TYPES
instance PPrint Body where
  pprint (E e)   = pprint e
  pprint (P p)   = pprint p
  pprint (R v p) = braces (pprint v <+> text "|" <+> pprint p)

-- instance PPrint a => Fixpoint (PPrint a) where
--   toFix (BDc c)  = toFix c
--   toFix (BTup n) = parens $ toFix n

-- MOVE TO TYPES
instance PPrint Def where
  pprint (Def m p c _ bs body) = pprint m <+> pprint (fst <$> p) <+> cbsd <> text " = " <> pprint body   
    where cbsd = parens (pprint c <> hsep (pprint `fmap` (fst <$> bs)))

-- MOVE TO TYPES
instance (PPrint t, PPrint a) => PPrint (Measure t a) where
  pprint (M n s defs) = pprint n <> text " :: " <> pprint s
                     $$ pprintLongList defs

-- MOVE TO TYPES
instance PPrint (Measure t a) => Show (Measure t a) where
  show = showpp

instance PPrint t => PPrint (CMeasure t) where
  pprint (CM n s) =  pprint n <> text " :: " <> pprint s

instance PPrint (CMeasure t) => Show (CMeasure t) where
  show = showpp


dataConTypes :: [Def] -> [(Id, SpecType)]
dataConTypes = concatMap go
  where
    go def = mkDataConIdsTy (ctor def) (defRefType def)

defRefType :: Def -> SpecType
defRefType (Def f args dc mt xs body) = uRType $ generalize $ mkArrow [] [] [] xts t'
  where 
    t   = fromMaybe (ofType $ dataConOrigResTy dc) mt
    xts = safeZipWith msg g xs $ ofType `fmap` dataConOrigArgTys dc
    g (x, Nothing) t = (x, t, mempty) 
    g (x, Just t)  _ = (x, t, mempty)
    t'  = mkForAlls args $ refineWithCtorBody dc f (fst <$> args) body t 
    msg = "defRefType dc = " ++ showPpr dc 

    mkForAlls xts t = foldl' (\t (x, tx) -> RAllE x tx t) t xts


refineWithCtorBody dc f as body t =
  case stripRTypeBase t of 
    Just (Reft (v, _)) ->
      strengthen t $ Reft (v, Refa $ bodyPred (EApp f (eVar <$> (as ++ [v]))) body)
    Nothing -> 
      errorstar $ "measure mismatch " ++ showpp f ++ " on con " ++ showPpr dc


bodyPred ::  Expr -> Body -> Pred
bodyPred fv (E e)    = PAtom Eq fv e
bodyPred fv (P p)    = PIff  (PBexp fv) p
bodyPred fv (R v' p) = subst1 p (v', fv)


-- | A wired-in measure @strLen@ that describes the length of a string
-- literal, with type @Addr#@.
strLen :: Measure SpecType id
strLen = M { name = dummyLoc "strLen"
           , sort = ofType (mkFunTy addrPrimTy intTy)
           , defs = []
           }

wiredInMeasures :: [Measure SpecType id]
wiredInMeasures = [strLen]

