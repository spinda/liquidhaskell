
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| data L [llen] = C (h :: Int) (t :: L) | N |]
[lq| invariant {v:L | (llen v) >= 0} |]

data L = C Int L | N

[lq| myFold :: forall <q :: L -> b -> Prop>.
              (as:L -> a:Int -> b<q as> -> b<q (C a as)>)
           -> b<q N>
           -> l:L
           -> b<q l>
  |]
myFold f z = go
  where
    go N       = z
    go (C a as) = f as a (go as)

[lq| measure llen :: L -> Int
    llen (N)      = 0
    llen (C x xs) = 1 + (llen xs)
  |]

[lq| qualif PappL(v:a, p:Pred a L , a:int, as:L ):
        papp2(p, v, C(a, as))
  |]
