
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell



bar = 0

[lq| assume (Prelude.++) :: [a] -> [a] -> [a] |]
