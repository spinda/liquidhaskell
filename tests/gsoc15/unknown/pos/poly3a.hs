
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


expand f []     = []
expand f (x:xs) = (f x) ++ (expand f xs)

baz :: a -> [Int]
baz _ = [0]

goo = expand baz

