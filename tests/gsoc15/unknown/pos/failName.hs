
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


bar = fail
  where fail = foo
        foo = 3 -- undefined


bar1 = fail ()
  where fail = foo
        foo = undefined
