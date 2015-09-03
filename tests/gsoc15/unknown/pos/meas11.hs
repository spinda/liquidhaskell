
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Data.Set (Set(..))

[lq| myfilter :: (a -> Bool) -> xs:[a] -> {v:[a] | Set_sub (listElts v) (listElts xs) } |]
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f []     = []
myfilter f (x:xs) = if f x 
                      then x : myfilter f xs 
                      else myfilter f xs

