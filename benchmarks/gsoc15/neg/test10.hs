{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| type That X = { v:Int | v == X } |]

thing :: [lq| (That ({1})) Int |]
thing = 1

