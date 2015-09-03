
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import qualified Data.Set

[lq| assume reverse :: xs:(UList a)
                   -> {v: UList a | (EqElts v xs)} 
  |]
[lq| type UList a = {v:[a] | (ListUnique v)} |]

[lq| predicate ListUnique LS =
       (Set_emp (listDup LS)) |]

[lq| predicate EqElts X Y =
       ((listElts X) = (listElts Y)) |]
[lq|
  measure listDup :: [a] -> (Data.Set.Set a)
  listDup([])   = {v | Set_emp v }
  listDup(x:xs) = {v | v = if (Set_mem x (listElts xs)) then (Set_cup (Set_sng x) (listDup xs)) else (listDup xs) }
  |]

[lq| foo :: xs:(UList a)
        -> {v: UList a | (EqElts v xs)} 
  |]


foo  = reverse 
