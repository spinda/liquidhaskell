
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| pos :: Nat -> Int |]
pos :: Int -> Int
pos = undefined

[lq| safeDiv :: Int -> {v:Int| v != 0} -> Int |]
safeDiv :: Int -> Int -> Int
safeDiv = undefined

[lq| LAZYVAR z |]
[lq| LAZYVAR z1 |]
[lq| LAZYVAR z2 |]
foo = if x > 0 then z else x
  where z  = z1 + z2
        z1 = 42 `safeDiv` x
        z2 = pos x
        x = choose 0
