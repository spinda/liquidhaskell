
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| measure getfst :: (a, b) -> a
    getfst (x, y) = x
  |]

[lq| type Pair a b   = {v0 : ({v:a | v = (getfst v0)}, b) | true } |]
[lq| type OPList a b = [(Pair a b)]<\h -> {v: (a, b) | (getfst v) >= (getfst h)}> |]
[lq| type OList a    = [a]<\h -> {v: a | (v >= h)}> |]

[lq| getFsts          :: OPList a b -> OList a |]
getFsts []           = [] 
getFsts ((x,_) : xs) = x : getFsts xs



