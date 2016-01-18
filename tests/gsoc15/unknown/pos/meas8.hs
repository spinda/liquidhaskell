
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| include <len.hquals> |]

[lq| measure rlen :: [a] -> Int 
    rlen ([])   = {v | v = 0}
    rlen (y:ys) = {v | v = (1 + rlen(ys))}
  |]

[lq| foo :: a -> {v:[b] | rlen(v) = 0} |]
foo x = []

[lq| mylen :: xs:[a] -> {v:Int | v = rlen(xs)} |]
mylen          :: [a] -> Int
mylen []       = 0
mylen (_:xs)   = 1 + mylen xs

[lq| mymap :: (a -> b) -> xs:[a] -> {v:[b] | rlen(v) = rlen(xs)} |]
mymap f []     = []
mymap f (x:xs) = (f x) : (mymap f xs)

