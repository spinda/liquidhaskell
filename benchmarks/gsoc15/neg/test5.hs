{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| thing :: Int -> x:Int |]
thing = undefined

