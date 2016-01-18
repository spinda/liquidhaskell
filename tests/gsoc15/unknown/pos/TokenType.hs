
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell



data TokenType = Foo | Char


[lq| bar :: Char |]
bar :: Char
bar = undefined
