
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Data.Set (Set(..)) 

[lq| include <selfList.hquals> |]

[lq| invariant {v0:[{v: a | (Set_mem v (listElts v0))}] | true } |]

[lq| type IList a  = {v0: [{v:a | (Set_mem v (listElts v0))}] | true } |]

[lq| moo :: [a] -> IList a |]
moo []     = [] 
moo (_:xs) = xs

goo []     = [] 
goo (_:xs) = xs

[lq| poo :: IList Int |]
poo = goo xs
  where 
    xs :: [Int]
    xs = [2,1,3,2]


