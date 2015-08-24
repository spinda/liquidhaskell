{-

I think this needs qualifiers to do the inference properly?

-}

{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell
import LiquidHaskell.Prelude

-- WORKS FINE
-- [lq|myabs :: Int -> Nat |]
myabs x = if x > 0 then x else (-x)

[lq|bob :: Int -> Nat |]
bob x = if x > 0 then x else (-x)

prop_abs1 = liquidAssertB (myabs   0  >= 0)
prop_abs2 = liquidAssertB (myabs   7  >= 0)
prop_abs3 = liquidAssertB (myabs (-3) >= 0)

[lq| qualif_Nat :: {v:Int | 0 <= v}|]
qualif_Nat = undefined 


