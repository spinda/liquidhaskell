{-# LANGUAGE QuasiQuotes #-}

module Test12 () where

import LiquidHaskell

data L a = N | C a (L a)

[lq| measure len :: L a -> Int |]
len N        = 0
len (C x xs) = 1 + len xs

[lq| three :: { v:(L Int) | len v = 3 } |]
three = C 1 (C 2 (C 3 N))

