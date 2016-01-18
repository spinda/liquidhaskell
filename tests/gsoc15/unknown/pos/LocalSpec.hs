
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude (choose)


prop = if x > 0 then bar x else x
  where x = choose 0
    [lq| bar :: Nat -> Nat |]
        bar :: Int -> Int
        bar x = x

[lq| bar :: a -> {v:Int | v = 9} |]
bar :: a -> Int
bar _ = 9
