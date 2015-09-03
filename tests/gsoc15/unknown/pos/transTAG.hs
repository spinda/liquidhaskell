
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| foo :: x: Int -> Int |]
foo :: Int -> Int
foo x = x + 1

[lq| transpose :: n:Int
              -> m:{v:Int | v > 0} 
              -> {v:[{v:[a] | len(v) = n}] | len(v) = m} 
              -> {v:[{v:[a] | len(v) = m}] | len(v) = n} 
  |]
transpose :: Int -> Int -> [[a]] -> [[a]]
transpose 0 _ _              = []
transpose n m ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (n - 1) m (xs : [t | (_:t) <- xss])
transpose n m ([] : _)       = liquidError "transpose1" 
transpose n m []             = liquidError "transpose2"

-- NEEDS TAGS: map head xss = [ h | (h:_) <- xss]
-- NEEDS TAGS: map tail xss = [t | (_:t) <- xss]


