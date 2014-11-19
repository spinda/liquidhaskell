{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.Haskell.Liquid.Variance (
    Variance(..), VarianceInfo
	) where

import Data.Typeable
import Data.Data

import Data.Monoid

import GHC.Generics

type VarianceInfo = [Variance]
data Variance = Invariant | Bivariant | Contravariant | Covariant deriving (Data, Typeable, Generic, Show)
