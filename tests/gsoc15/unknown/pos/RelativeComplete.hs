
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


{-@ LIQUID "--no-termination "@-}

-- Here p and q of `app` will be instantiated to 
-- p , q := \v -> i <= v

main i = app (check i) i

[lq| check :: x:Int -> {v:Int | x <= v} -> () |]
check :: Int -> Int -> ()
check = undefined


[lq| app :: forall <p :: Int -> Prop, q :: Int -> Prop>. 
           {Int<q> <: Int<p>}
           {x::Int<q> |- {v:Int| v = x + 1} <: Int<q>}
           (Int<p> -> ()) -> x:Int<q> -> () |]
app :: (Int -> ()) -> Int -> ()
app f x = if p x then app f (x + 1) else f x


p :: a -> Bool
[lq| p :: a -> Bool |]
p = undefined
