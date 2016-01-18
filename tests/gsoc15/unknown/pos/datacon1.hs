
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data Foo a = F a a a 

z :: Foo Int
z = F 1 2 3 

