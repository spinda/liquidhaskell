
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| type PlusOne = (Int, Int)<{\x v -> v > x}> |]

[lq| plusOne :: PlusOne |]
plusOne :: (Int, Int)
plusOne = (0, 1)

[lq| plusOnes :: Maybe PlusOne |]
plusOnes :: Maybe (Int, Int) 
plusOnes = Just plusOne -- (0, 1) (5,6), (999,1000)]
