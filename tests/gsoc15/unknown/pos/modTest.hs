
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude (isEven)

[lq| takeEvens :: [Int] -> [{v: Int | v mod 2 = 0}] |]
takeEvens :: [Int] -> [Int]
takeEvens []     = []
takeEvens (x:xs) = if isEven x
                     then x : takeEvens xs 
                     else takeEvens xs 
