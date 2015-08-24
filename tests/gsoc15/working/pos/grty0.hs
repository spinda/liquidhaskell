{-# LANGUAGE QuasiQuotes #-}

module Test () where

import LiquidHaskell
import LiquidHaskell.Prelude

[lq| stupid :: Int -> Int |]
stupid x = 0

[lq| myId :: x:a -> {v:a | v = x } |]
myId x = x 

[lq| single :: a -> {v: [a] | len v > 0} |]
single x = [x] 

