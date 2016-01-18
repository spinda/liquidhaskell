
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| measure lllen |]
llen :: [a] -> Int
llen [] = 0
llen (x:xs) = 1 + llen xs


[lq| llen, llllen :: xs:[a] -> {v:Int| (lllen xs) = v} |]

lllen :: [a] -> Int	
lllen [] = 0
lllen (x:xs) = 1 + lllen xs


llllen = lllen
