
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| using [a] as {v : [a] | (len v) > 0 } |]


xs = repeat 1

add x xs = x:xs

bar xs = head xs
foo xs = tail xs
