{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell

type Nat = [lq| {v:Int | 0 <= v} |]

bad :: Int -> Nat
bad x = x + 1

