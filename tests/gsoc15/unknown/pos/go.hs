
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| invariant {v:Int | v >= 0} |]

[lq| qualif Sum(v:Int, x: Int, y: Int): v = x + y |]

[lq| invariant {v:Int | v >= 0} |]

[lq| foo  :: x:Int -> {v:Int | v = x} |]
foo x    = go x 0
  where 
    go     :: Int -> Int -> Int 
    go 0 m = m
    go n m = go (n-1) (m+1)


poop x = foo x 



