
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| foo :: {v:String | len v = 3} |]
foo = "foo"
