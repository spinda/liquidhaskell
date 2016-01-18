
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| foo :: Eq b => a -> b -> a |]
foo :: Eq b => a -> b -> a
foo = undefined
