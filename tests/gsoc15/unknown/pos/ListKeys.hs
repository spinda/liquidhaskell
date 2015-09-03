
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell

import Data.Set (Set(..)) 

[lq|  measure listKeys :: [(k, v)] -> (Set k) 
    listKeys([])   = {v | Set_emp v }
    listKeys(x:xs) = {v | v = (Set_cup (Set_sng (fst x)) (listKeys xs)) }

|]


[lq| getFsts :: ys:[(a, b)] -> {v : [a] | listElts v = listKeys ys } |]
getFsts ::[(a, b)] ->  [a]
getFsts []           = []
getFsts ((x, _): xs) = x : getFsts xs






