
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import qualified Mod1 as M

[lq| inc :: x:M.Goo -> {v: M.Goo | (myg v) > (myg x)} |]
inc (M.G x) = M.G (x + 1)

