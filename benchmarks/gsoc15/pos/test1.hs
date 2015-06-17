{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell

[lq| type Nat = {v:Int | 0 <= v} |]

type Plus x y = [lq|{v:Int | v = x + y}|]

plus :: [lq| x:Int -> y:Int -> Plus x y |]
plus = undefined

ok :: Nat -> Nat
ok x = plus x one

one :: [lq|{v:Int | v = 1}|]
one = undefined

