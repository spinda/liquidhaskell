{-# LANGUAGE QuasiQuotes #-}

module Test4 () where

import LiquidHaskell

[lq| thing1 :: a -> a |]
thing1 x = x

[lq| thing2 :: Int -> a -> a |]
thing2 _ x = x

[lq| thing3 :: a -> a |]
thing3 x = x

