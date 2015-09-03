
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| type Vec a N = {v:[a] | len v = N } |]

[lq| ok :: Vec Int 3 |]
ok = [1,2,3] :: [Int]

[lq| ok' :: Vec _ 3 |]  -- would be nice to support the hole in the application..
ok'     = [1,2,3]



