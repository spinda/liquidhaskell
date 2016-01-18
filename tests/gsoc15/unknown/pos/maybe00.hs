
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


gloop = poop True

[lq| poop :: z:a -> {v: Maybe a | fromJust(v) = z} |]
poop z = Just z


