{-# LANGUAGE QuasiQuotes #-}

module Abs () where

import LiquidHaskell

[lq| absN :: (Num a, Ord a) => x:a -> { v:a | v = if x > 0 then x else -x } |]
absN x = if x > 0 then x else (0 - x)

[lq| absI :: x:Int -> { v:Int | v= if x> 0 then x else -x } |]
absI x = if x > 0 then x else (0 - x)

