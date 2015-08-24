{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| type Pos = {v:Int | 0 < v} |]

[lq| bad :: Pos -> Pos |]
bad x = 0

