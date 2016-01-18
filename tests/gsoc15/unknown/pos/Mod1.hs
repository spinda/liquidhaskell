
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data Goo = G Int

[lq| measure myg :: Mod1.Goo -> Int 
    myg (Mod1.G n) = n
  |]
 
[lq| inc :: x:Goo -> {v: Goo | (myg v) > (myg x)} |]
inc (G x) = G (x + 1)


