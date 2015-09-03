
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| type Range Lo Hi = {v:Int | Lo <= v && v < Hi} |]

[lq| bow :: Range 0 100 |]
bow :: Int
bow = 12
