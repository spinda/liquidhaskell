
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Prelude hiding (map, foldl)

data L a = N | C a (L a)
[lq| 
data L [llen] a = N | C (x::a) (xs::(L a))
  |]

[lq| measure llen :: (L a) -> Int
    llen(N) = 0
    llen(C x xs) = 1 + (llen xs)
  |]


[lq|map :: (a -> b) -> [a] -> [b]|]
map f []     = []
map f (x:xs) = f x : map f xs
 
bar = map id [1, 2]

[lq| Decrease go 2 |]
rev xs = go [] xs
  where go ack  []    = ack
        go ack (x:xs) = go (x:ack) xs

[lq| invariant {v:(L a) | ((llen v) >= 0)}|]

mapL f N = N
mapL f (C x xs) = C (f x) (mapL f xs)


foldl f z [] = z
foldl f z (x:xs) = x `f` foldl f z xs

