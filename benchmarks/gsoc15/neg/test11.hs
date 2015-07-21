{-# LANGUAGE QuasiQuotes #-}

module Test11 () where

import LiquidHaskell

data L a = N | C a (L a)

[lq| measure len :: L a -> Int |]
len N        = 0
len (C x xs) = 1 + len xs

[lq| empty :: { v:(L Int) | len v = 0 } |]
empty = C 1 N

