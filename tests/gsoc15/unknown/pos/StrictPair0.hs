
{-# LANGUAGE QuasiQuotes #-}
-- Compare with tests/pos/StrictPair1.hs



import LiquidHaskell


import Language.Haskell.Liquid.Prelude (liquidAssert)

[lq| measure tsnd :: (a, b) -> b 
    tsnd (x, y) = y 
  |] 

[lq| type Foo  a = ((a, Int), Int)<{\z v -> v <= (tsnd z)}> |]

[lq| poo :: (Foo a) -> () |]
poo     :: ((a, Int), Int) -> ()
poo ((x, n), m) = liquidAssert (m <= n) () 
