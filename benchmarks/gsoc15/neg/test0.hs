{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test0 () where

import LiquidHaskell

[lq| embed Int as int |]

type Pos = [lq| {v:Int | 0 < v} |]

bad :: Pos -> Pos
bad x = 0

