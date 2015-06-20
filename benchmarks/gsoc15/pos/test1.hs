{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test1 () where

import LiquidHaskell

[lq| type Nat = {v:Int | 0 <= v} |]
[lq| type Plus x y = {v:Int | v = x + y} |]

ok :: Nat -> Nat
ok x = plus x one

plus :: [lq| x:Int |] -> [lq| y:Int |] -> Plus x y
plus = undefined

one :: [lq|{v:Int | v = 1}|]
one = undefined

