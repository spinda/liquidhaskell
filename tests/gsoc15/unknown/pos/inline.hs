
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| inline eqq |]
eqq :: Ord a => a -> a -> Bool
eqq x y = x > y

eqqtest :: Ord a => a -> a -> Bool
[lq| eqqtest :: Eq a => x:a -> y:a -> {v:Bool | Prop v <=> (eqq x y) } |]
eqqtest x y = x > y

[lq| inline mymax |]
[lq| inline mymin |]


mymax, mymin :: Ord a => a -> a -> a
mymax x y = if x >= y then x else y


mymin x y = mymax y x

[lq| measure foo |]
foo :: Ord a => D a -> a
foo (D x y) = mymax x y
foo (F x)   = x

bar :: Ord a => D a -> a
[lq| bar :: Ord a => x:(D a) -> {v:a |  v = bar x} |]
bar (D x y) = mymax x y
bar2 :: Ord a => D a -> a
[lq| bar2 :: Ord a => x:(D a) -> {v:a |  v = bar2 x} |]
bar2 (D x y) = mymin y x




foooo = D 


[lq| measure bar :: (D a) -> a
    bar(D x y) = (mymax x y) 
  |]

[lq| measure bar2 :: (D a) -> a
    bar2(D x y) = (mymin x y) 
  |]

data D a = D a a | F a


[lq| mymax3, mymax :: x:a -> y:a -> {v:a | v = mymax x y} |]
mymax3 :: Ord a => a -> a -> a
mymax3 x y = if x >= y then x else y


