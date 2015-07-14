{-# LANGUAGE QuasiQuotes #-}

module Test10 where

import LiquidHaskell

id' :: [lq| x:a -> { v:a | x = v } |]
id' = id

