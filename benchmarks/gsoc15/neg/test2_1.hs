{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell

type Nat = [lq| { v:Int | 0 <= v } |]

bad :: Nat -> Nat
bad x = -1 + x

