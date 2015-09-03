
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| pos :: Nat -> Int |]
pos :: Int -> Int
pos = undefined


[lq| LAZYVAR z |]
foo = if x > 0 then z else x
  where z = pos x
        x = choose 0
