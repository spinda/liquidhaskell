
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| measure getFoo :: Foo -> Int
    getFoo (Foo x) = x
  |]

data Foo = Foo Int

data Bar = A | B | C
