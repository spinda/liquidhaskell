{-# LANGUAGE UndecidableInstances  #-}

module Language.Haskell.Liquid.Serialize (
    encode
  , decode
  ) where

import Data.Serialize                           (Serialize(..), encode, decode)
import Data.Serialize.Text                      ()

import Control.Applicative                      ((<$>))

import Data.Hashable

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

import Data.Interned.Text                       (InternedText)

import Language.Fixpoint.Types as Fx

import qualified Language.Haskell.Liquid.Types    as Ty
import qualified Language.Haskell.Liquid.Variance as Vr

instance (Eq k, Hashable k, Serialize k, Serialize v) => Serialize (M.HashMap k v) where
  put = put . M.toList
  get = M.fromList <$> get

instance (Eq v, Hashable v, Serialize v) => Serialize (S.HashSet v) where
  put = put . S.toList
  get = S.fromList <$> get

instance Serialize InternedText

-- Don't bother encoding location information. Just use a dummyLoc when decoding.
instance Serialize a => Serialize (Ty.Located a) where
  put = put . Ty.val
  get = dummyLoc <$> get

instance Serialize Fx.Symbol

instance Serialize Fx.Reft
instance Serialize Fx.Refa
instance Serialize Fx.Pred
instance Serialize Fx.Subst

instance Serialize Fx.Expr
instance Serialize Fx.Bop
instance Serialize Fx.Brel
instance Serialize Fx.Constant
instance Serialize Fx.Sort
instance Serialize Fx.SymConst
instance Serialize Fx.FTycon

instance (Serialize c, Serialize tv, Serialize r, Serialize (Ty.RTProp c tv r), Serialize (Ty.RTProp c tv ())) => Serialize (Ty.RType c tv r)

instance (Serialize τ, Serialize r, Serialize t) => Serialize (Ty.Ref τ r t)

instance Serialize a => Serialize (Ty.PVar a)
instance Serialize t => Serialize (Ty.PVKind t)
instance Serialize t => Serialize (Ty.World t)
instance Serialize t => Serialize (Ty.HSeg t)
instance Serialize r => Serialize (Ty.UReft r)
instance Serialize a => Serialize (Ty.Def a)
instance Serialize t => Serialize (Ty.RClass t)

instance Serialize Ty.Predicate
instance Serialize Ty.Oblig
instance Serialize Ty.Stratum

instance Serialize Ty.Body

instance Serialize Ty.BTyCon
instance Serialize Ty.TyConInfo
instance Serialize Ty.SizeFn

instance Serialize Vr.Variance

