
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


{-@ LIQUID "--totality "@-}

[lq| predicate NonNull X = ((len X) > 0) |]

[lq| risers :: (Ord a) => zs:[a] -> {v: [[a]] | ((NonNull zs) => (NonNull v)) } |] 
risers []        
  = []
risers [x]       
  = [[x]]
risers (x:y:etc) 
  = if x <= y then (x:s):ss else [x]:(s:ss)
    where (s:ss) = risers (y:etc)
