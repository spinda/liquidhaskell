{-# LANGUAGE UndecidableInstances  #-}

module Language.Haskell.Liquid.Serialize (
    encode
  , decode
  )
  where

import Data.Serialize                           (Serialize(..), encode, decode)
import Data.Serialize.Text                      ()

import Control.Applicative                      ((<$>))
import Control.Monad                            (liftM3)

import Data.Hashable

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

import Data.Interned.Text                       (InternedText)
import Text.Parsec.Pos                          (SourcePos, sourceName, sourceLine, sourceColumn, newPos)

import Language.Fixpoint.Names as Fx            (Symbol)
import Language.Fixpoint.Types as Fx

import qualified Language.Haskell.Liquid.Types   as Ty

instance (Eq k, Hashable k, Serialize k, Serialize v) => Serialize (M.HashMap k v) where
  put = put . M.toList
  get = M.fromList <$> get

instance (Eq v, Hashable v, Serialize v) => Serialize (S.HashSet v) where
  put = put . S.toList
  get = S.fromList <$> get

instance Serialize InternedText

instance Serialize SourcePos where
  put pos = put (sourceName pos) >> put (sourceLine pos) >> put (sourceColumn pos)
  get = liftM3 newPos get get get

instance Serialize Fx.Symbol
instance Serialize Fx.Qualifier

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

instance (Serialize tv, Serialize ty) => Serialize (Ty.RTAlias tv ty)

instance Serialize a => Serialize (Ty.PVar a)
instance Serialize t => Serialize (Ty.PVKind t)
instance Serialize t => Serialize (Ty.World t)
instance Serialize t => Serialize (Ty.HSeg t)
instance Serialize r => Serialize (Ty.UReft r)
instance Serialize a => Serialize (Ty.Located a)
instance Serialize a => Serialize (Ty.Def a)
instance Serialize t => Serialize (Ty.RClass t)

instance Serialize Ty.Predicate
instance Serialize Ty.Oblig
instance Serialize Ty.Stratum

instance Serialize Ty.Body
