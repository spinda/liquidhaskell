
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell



[lq| foo :: Fractional a => {v:a | v >= 1.0} |]
foo :: Fractional a => a
foo = undefined

[lq| foo'' :: RealFloat a => {v:a | v >= 1.0} |]
foo'' :: RealFloat a => a
foo'' = undefined


[lq| foo' :: Num a => {v:a | v >= 1} |]
foo' :: Num a => a
foo' = undefined