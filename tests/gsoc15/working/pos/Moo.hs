{-# LANGUAGE QuasiQuotes #-}

module Moo (plusOne) where

import LiquidHaskell

[lq| plusOne :: x:Int -> {v:Int| v = x + 1 } |]
plusOne x = x + 1

