
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| type OList a = [a]<{\fld v -> v >= fld}> |]

[lq| insertSort    :: (Ord a) => xs:[a] -> {v : OList a | len(v) = len(xs)} |]
insertSort        :: (Ord a) => [a] -> [a]
insertSort []     = []
insertSort (x:xs) = insert x (insertSort xs)

[lq| insertSort'    :: (Ord a) => xs:[a] -> OList a |]
insertSort'        :: (Ord a) => [a] -> [a]
insertSort' xs      = foldr insert [] xs


[lq| insert      :: (Ord a) => x:a -> xs: OList a -> {v: OList a | len(v) = (1 + len(xs)) } |]
insert y []                   = [y]
insert y (x : xs) | y <= x    = y : x : xs
                  | otherwise = x : insert y xs
