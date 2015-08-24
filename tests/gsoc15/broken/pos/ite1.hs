{-

Strange Fixpoint parse error:

syntax error at ./.liquid/ite1.hs.fq: 1.53
Fatal error: exception Parsing.Parse_error

-}

{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| myabs :: x:Int -> {v: Int | v = if x > 0 then x else -x } |]
myabs x | x > 0     = x
        | otherwise = (-x)

