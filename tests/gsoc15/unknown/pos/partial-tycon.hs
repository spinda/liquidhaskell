
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data Id a = Id a

data Foo m a = Foo (m a)

foo :: Foo Id a
foo = undefined
