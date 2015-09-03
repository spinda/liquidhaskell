
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude (liquidAssert)

data Vec a = V a deriving (Eq)

-- this is verified ...
[lq| bar :: x:Int 
        -> y:{v: Int | v = x } 
        -> Int 
  |]
bar     :: Int -> Int -> Int
bar x y = liquidAssert (x == y) 0

-- and so is this...

[lq| goo :: x:[Int] 
        -> y:{v: [Int] | v = x } 
        -> Int 
  |]
goo     :: [Int] -> [Int] -> Int
goo x y = liquidAssert (x == y) 0


-- BUT THIS IS NOT!!
[lq| foo :: x: (Vec Int)
        -> y:{v: Vec Int | v = x } 
        -> Int 
  |]
foo     :: Vec Int -> Vec Int -> Int
foo x y = liquidAssert (x == y) 0



