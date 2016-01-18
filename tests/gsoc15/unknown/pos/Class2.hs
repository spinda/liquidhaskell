
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


{-@ LIQUID "--idirs=../neg "@-}
import Class5

instance Foo ()
