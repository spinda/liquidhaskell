{-# LANGUAGE QuasiQuotes #-}

module Test10 where

import LiquidHaskell

[lq| id' :: x:a -> { v:a | x = v } |]
id' = id

