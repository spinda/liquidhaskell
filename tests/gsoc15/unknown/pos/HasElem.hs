
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


{-@ LIQUID "--no-termination "@-}

data L a = Nil | Cons a (L a)

[lq| measure hasElem |]
hasElem :: Eq a => a -> L a -> Bool
hasElem x Nil = False
hasElem x (Cons y ys) = x == y || hasElem x ys

[lq| prop :: {v:Bool | Prop v <=> true} |]
prop :: Bool
prop = hasElem 1 (Cons 1 Nil)

[lq| prop1 :: {v:Bool | Prop v <=> false} |]
prop1 :: Bool
prop1 = hasElem 1 (Cons 2 Nil)

[lq| prop2 :: {v:Bool | Prop v <=> false} |]
prop2 :: Bool
prop2 = hasElem 1 Nil
