
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell

import Prelude hiding (repeat, take)

data L a = N | C a (L a)

[lq|
data L a <p :: (L a) -> Prop>
  = N
  | C (x::a) (xs::L <p> a <<p>>)
|]

[lq|
measure isCons :: L a -> Prop
isCons (N)     = false
isCons (C a l) = true
|]

[lq| type Stream a = {v: L <{\v -> (isCons v)}> a | (isCons v)} |]

[lq| Lazy repeat |]
[lq| repeat :: a -> Stream a |]
repeat :: a -> L a
repeat x = C x (repeat x)

[lq| take :: Nat -> Stream a -> L a |]
take :: Int -> L a -> L a
take 0 _           = N
take n ys@(C x xs) = x `C` take (n-1) xs
