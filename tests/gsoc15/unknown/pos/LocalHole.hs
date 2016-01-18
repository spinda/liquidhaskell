
{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--no-termination "@-}


import LiquidHaskell


[lq| assume (!!) :: xs:[a] -> {v:Nat | v < len xs} -> a |]

mysum xs = go 0 0
  where
    n = length xs
    [lq| go :: i:{Nat | i <= n} -> _ -> _ |]
    go i acc
      | i >= n    = acc
      | otherwise = go (i+1) (acc + xs !! i)
