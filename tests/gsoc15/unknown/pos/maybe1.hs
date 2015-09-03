
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data MaybeS a = NothingS | JustS !a

[lq| measure isJustS :: forall a. MaybeS a -> Prop
    isJustS (JustS x)  = true
    isJustS (NothingS) = false
  |]

[lq| measure fromJustS :: forall a. MaybeS a -> a 
    fromJustS (JustS x) = x 
  |]

[lq| type OList a = [a]<{\fld v -> (v >= fld)}> |]

[lq| filterGt :: (Ord a) => x:MaybeS a -> OList a -> OList {v:a | ((isJustS(x)) => (fromJustS(x) <= v)) } |]

filterGt ::  Ord a => MaybeS a -> [a] -> [a]
filterGt NothingS  xs = xs
filterGt (JustS x) xs = foo' x xs
  where foo' y []     = []
        foo' y (x:xs) = case compare y x of 
                          GT -> foo' y xs 
                          LT -> x:xs 
                          EQ -> xs 
