{-

Specification for (.) need abs refinements

-}

{-# LANGUAGE QuasiQuotes #-}

module Loo where

import LiquidHaskell

import qualified Goo as G

[lq| pp :: z:Int -> {v:Int| v > z} |]
pp x = G.plusOne (G.plusOne x)

[lq| plusThree :: x:Int -> {v:Int| v = x + 3 } |]
plusThree = G.plusOne . G.plusTwo

[lq| plusFour :: x:Int -> {v:Int| v = x + 4 } |]
plusFour  = G.plusTwo . G.plusTwo

