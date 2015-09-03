
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Prelude hiding (return, (>>=))

data ST s a = S (s -> (a, s))
[lq| data ST s a <p :: s -> Prop> 
     = S (x::(f:s<p> -> (a, s<p>))) 
  |]

[lq| foo :: (Int, {v:Int|v >=0})|]
foo = apply action 0

[lq| action :: ST <{\v -> v>=0 }> Int Int|]
action :: ST Int Int
action
 = act1 `comp` \n1 -> 
   act2 `comp` \n2 -> 
   return n1


[lq| act1 :: ST <{\v -> v>=0 } > Int Int |]
act1 :: ST Int Int
act1 = S (\n -> (n, n+1))


act2 :: ST Int Int
act2 = S (\n -> (n, n+9))



[lq|
apply :: forall <p :: s -> Prop>.
          ST <p> s a -> f:s<p> -> (a, s <p>)
  |]
apply :: ST s a -> s -> (a, s)
apply (S f) x = f x

[lq|
return :: forall <p:: s -> Prop>.
          x:a -> ST <p> s {v:a|v=x}
  |]
return ::  a -> ST s a
return x = S $ \s -> (x, s)

[lq|
comp :: forall < p :: s -> Prop>.
    ST <p> s a -> (a -> ST <p> s b) -> ST <p> s b
|]
comp :: ST s a -> (a -> ST s b) -> ST s b
(S m) `comp` k 
  = S $ \s -> case (m s) of { (r, new_s) -> 
              case (k r) of { S k2 -> 
                (k2 new_s) }}


