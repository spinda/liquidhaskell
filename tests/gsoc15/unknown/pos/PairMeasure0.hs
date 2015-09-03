
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| foo :: p:(a, b) -> {v:a | (v = (fst p))} |]
foo (x, y) = x

