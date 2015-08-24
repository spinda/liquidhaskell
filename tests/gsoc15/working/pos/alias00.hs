{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell
import LiquidHaskell.Prelude

[lq| type PosInt = {v: Int | v >= 0} |]

[lq| myabs :: Int -> PosInt |]
myabs x = if x > 0 then x else (-x)

[lq| type NNList a = {v: [a] | len v > 0} |]

[lq| single :: a -> NNList a |]
single x = [x] 

