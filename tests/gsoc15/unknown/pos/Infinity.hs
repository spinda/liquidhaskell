
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude
{-@ LIQUID "--totality "@-}
[lq| Lazy inf |]

[lq| inf :: {v:[Int] | (((len v) > oo) && ((len v) > 2))} |]
inf :: [Int]
inf = 1 : inf

bar = tail $ tail inf


foo = inf !! n
  where n = myabs $ choose 0

myabs :: Int -> Int
[lq| myabs :: Int -> {v:Int | v >= 0} |]
myabs = undefined

-- Encoding infinity.....

[lq| measure oo :: Int |]
[lq| invariant {v:Int | (v < oo) }|]
