
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| inline implies |]
implies p q = (not p) || q

