{-
   Chris Tetreault: LH doesn't seem to know what to do with the <> inside of a [lq| block |]
   the old style LH can handle this source fine
-}

{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--no-warnings" @-}
{-@ LIQUID "--no-termination" @-}




import LiquidHaskell


data List a = N | C a (List a)

infixr 9 `C`

[lq| ifoldr :: forall a b <p :: List a -> b -> Prop>.
                 (xs:_ -> x:_ -> b<p xs> -> b<p(C x xs)>)
               -> b<p N>
               -> ys:List a
               -> b<p ys>                            |]
ifoldr :: (List a -> a -> b -> b) -> b -> List a -> b
ifoldr = undefined

[lq| data List a <p :: a -> a -> Prop>
     = N | C {x :: a, xs :: List<p> a<p x>} |]

[lq| type IncrList a = List <{\x y -> x <= y}> a |]

[lq| insert :: a -> IncrList a -> IncrList a |]
insert :: a -> List a -> List a
insert = undefined

[lq| insertSort      :: xs:List a -> {v:IncrList a | true } |]
insertSort :: List a -> List a
insertSort = undefined



nil :: List a
nil = N
