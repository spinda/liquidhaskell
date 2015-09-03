
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data T =  A | B

[lq| invariant {v:T | (v = A || v = B)} |]

thisIsA = A
thisIsB = B
