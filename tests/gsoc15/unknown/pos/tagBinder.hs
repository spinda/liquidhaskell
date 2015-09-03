
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data Foo a = F Int a

[lq| data Foo a = F {tag :: Int, f :: a} |]

foo = F
