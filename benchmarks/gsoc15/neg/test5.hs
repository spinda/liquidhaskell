{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

thing :: [lq| Int -> x:Int |]
thing = undefined

