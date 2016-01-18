
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| qualif Poo(v:a, x:a, y:a): (len v) = (len x) + (len y) |]

[lq| Decrease go 2 |]

[lq| rev :: xs:[a] -> {v: [a] | (len v) = (len xs)} |]
rev = go [] 
  where 
    go acc []     = acc
    go acc (x:xs) = go (x:acc) xs
