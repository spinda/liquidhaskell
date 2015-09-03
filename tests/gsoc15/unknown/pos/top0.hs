
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| invariant {v:Int | v >= 0} |]

[lq| qualif Sum(v:Int, x: Int, y: Int): v = x + y |]

-- | This should get a TOP type
poop x = zoo x 

-- | This is USED but should ALSO get a TOP type (since exported)
loop x     = go x 0
  where 
    go     :: Int -> Int -> Int 
    go 0 m = m
    go n m = go (n-1) (m+1)

zoo     = loop

loop' x     = go x 0
  where 
    go     :: Int -> Int -> Int 
    go 0 m = m
    go n m = go (n-1) (m+1)

-- | This HAS a sig so it should NOT get a TOP type
[lq| zoo' :: x:Int -> {v:Int | v = x} |]
zoo'     = loop'
