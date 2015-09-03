
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import qualified ResolveA as RA
import qualified ResolveB as RB


[lq| x :: {v:RB.Bar | ((v = RB.B) && (NotA v))} |]
x = RB.B
