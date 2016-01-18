{- chris:

zipSO.hs:16:5:

    "zipSO.hs" (line 16, column 6):
    [lq| Decrease go 2 |]
         ^
    unexpected 'D'
    expecting "embed", "inline", "bound", "measure", "qualif", "type", "assume", "(", variable identifier or end of input
-}


{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


-- From
-- http://stackoverflow.com/questions/17501777/implementing-a-zipper-for-length-indexed-lists/17503667#17503667

import Prelude hiding ((++))

[lq| zipper :: zs:[a] -> [(a, {v:[a] | (len v) = (len zs) - 1})] |]
zipper zs          = go [] zs

[lq| Decrease go 2 |]
[lq| go :: prev:[a] -> rest:[a] -> [(a, {v:[a] | (len v) = (len prev) + (len rest) - 1})] |]
go _    []     = []
go prev (x:xs) = (x, prev ++ xs) : go (prev ++ [x]) xs

[lq| append :: xs:[a] -> ys:[a] -> {v:[a] | (len v) = (len xs) + (len ys)} |]
append [] ys     = ys
append (x:xs) ys = x : append xs ys

(++) = append
