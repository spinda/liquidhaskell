
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Prelude hiding (snd, fst)

data ST a s = S (s -> (a, s))
[lq| data ST a s <post :: s -> a -> s -> Prop> 
       = S (ys::(x:s -> ((a, s)<\xx -> {v:s<post x xx> | true} > )))
  |]

[lq| returnST :: xState:a 
             -> ST <{\xs xa v -> (xa = xState)}> a s 
  |]

returnST :: a -> ST a s
returnST x = S $ \s -> (x, s)
