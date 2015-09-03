
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Prelude hiding (snd, fst)

data ST a s = S (s -> (a, s))
[lq| data ST a s <pre :: s -> Prop, post :: a -> s -> Prop> 
       = S (ys::(x:s<pre> -> ((a, s)<post>)))
  |]

[lq| returnST :: forall <pre :: s -> Prop, post :: a -> s -> Prop>.
               xState:a 
           -> ST <{v:s<post xState>| true}, post> a s
  |]
returnST :: a -> ST a s
returnST x = S $ \s -> (x, s)


[lq| bindST :: forall <pbind :: s -> Prop, qbind :: a -> s -> Prop, rbind :: b -> s -> Prop>.
            ST <pbind, qbind> a s 
         -> (xbind:a -> ST <{v:s<qbind xbind> | true}, rbind> b s) 
         -> ST <pbind, rbind> b s
 |]
bindST :: ST a s -> (a -> ST b s) -> ST b s
bindST (S m) k = S $ \s -> let (a, s') = m s in apply (k a) s'

[lq| apply :: forall <p :: s -> Prop, q :: a -> s -> Prop>.
             ST <p, q> a s -> s<p> -> (a, s)<q>
  |]
apply :: ST a s -> s -> (a, s)
apply (S f) s = f s
