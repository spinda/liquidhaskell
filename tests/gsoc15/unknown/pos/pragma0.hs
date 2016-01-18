
{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--notermination "@-}



import LiquidHaskell



-- an obviously non-terminating function
zoo   :: Int -> Int
zoo x = zoo x
