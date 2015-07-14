{-# LANGUAGE QuasiQuotes #-}

module Test4 () where

import LiquidHaskell

thing1 :: a -> [lq| a |]
thing1 x = x

thing2 :: Int -> [lq| a |] -> a
thing2 _ x = x

thing3 :: [lq| a -> a |]
thing3 x = x

[lq| thing4 :: a -> a |]
thing4 x = x

