
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| data LL a = BXYZ { size  :: {v: Int | v > 0 }
                     , elems :: {v: [a] | (len v) = size }
                     }
  |]

data LL a = BXYZ { size  :: Int
                 , elems :: [a]
                 }

[lq| mk :: a -> Int -> LL a |]
mk x n | n > 0     = BXYZ n (clone x n) 
       | otherwise = BXYZ 1 [x]

[lq| bk :: LL a -> {v: Int | v > 0} |]
bk (BXYZ n xs) = liquidAssert (length xs == n) n

[lq| clone :: x:a -> n:Int -> {v:[a]| (len v) = n} |]
clone :: a -> Int -> [a]
clone = error "FOO"
