
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


foo :: [a] -> (b -> c) -> (b -> c)
foo []     f = f
foo (x:xs) f = foo xs f

