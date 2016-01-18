
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| assume foo :: a -> a |]
foo :: a -> a
foo f = foo f
