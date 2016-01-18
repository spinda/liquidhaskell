
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}



import LiquidHaskell
 

import Prelude hiding (sum)

[lq| sum :: Nat -> Nat |]
sum   :: Int -> Int
sum 0 = 0
sum n = n + sum (n-1)

[lq| fib :: Nat -> Nat |]
fib :: Int -> Int 
fib 0 = 1
fib 1 = 1 
fib n = fib (n-1) + fib (n-2)

[lq| sumUp :: Nat -> Nat |]
sumUp :: Int -> Int
sumUp n  = go n 0 0
  where 
    go (d :: Int) acc i
      | i < n     = go (d - 1) (acc + i) (i + 1) 
      | otherwise = acc

[lq| qualif Diff(v:Int, x:Int, y:Int): v = x - y |] 

[lq| nonTerm :: Nat -> Nat |]
nonTerm :: Int -> Int
nonTerm n = nonTerm (n+1)

[lq| Lazy nonTerm |]

