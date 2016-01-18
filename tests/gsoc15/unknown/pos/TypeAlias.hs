
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data Foo a b = Foo a b

type Bar = Foo Int


[lq| foo :: String  |]
foo :: String
foo = "mpla"

[lq| bar :: Bar {v:Int | v = 2} |]
bar :: Bar Int
bar = Foo 1 2
