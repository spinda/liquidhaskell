
{-# LANGUAGE QuasiQuotes #-}

-- small test for playing around with exports.



import LiquidHaskell


import Language.Haskell.Liquid.Prelude

data Goo = G Int

[lq| data Goo = G (x :: {v:Int | v > 0}) |]

[lq| f :: Goo -> Goo |] 
f (G n) 
  | n > 0     = G (n +  1)
  | otherwise = liquidError "ad"

