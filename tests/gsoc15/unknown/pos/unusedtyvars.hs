
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data F a b c = F (Int -> b -> c)
{- data F a b c = F (x::(Int -> b -> c)) @-}


[lq| bar :: F {v:Int| v >= 0} b c |]
bar :: F Int b c
bar = undefined


[lq| foo :: F {v:Int| v >= 0} b c  -> Int |]
foo :: F Int b c -> Int
foo = undefined

[lq| hoo :: Int |]
hoo = foo bar
