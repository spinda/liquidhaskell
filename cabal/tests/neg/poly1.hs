module Poly0 where

import LiquidPrelude

myabs x    = if x `gt` 0 then x else 0 `minus` x

myid2 a b  = a  

----------------------------------------------------------

x =  choose 0

prop_id4 = let x'  = myabs x in 
           let x'' = myid2 x' [] in 
           assert (x'' `geq` 10)

prop_id5 = assert (x'' `geq` 0)
  where x'  = myabs x 
        x'' = myid2 x' [] 
