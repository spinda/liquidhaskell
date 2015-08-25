{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell
import LiquidHaskell.Prelude
import LiquidHaskell.Qualifiers

myabs x = if x > 0 then x else (-x)

prop_abs1 = liquidAssertB (myabs   0  >= 0)
prop_abs2 = liquidAssertB (myabs   7  >= 0)
prop_abs3 = liquidAssertB (myabs (-3) >= 0)

