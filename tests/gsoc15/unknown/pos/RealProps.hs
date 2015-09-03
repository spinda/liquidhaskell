
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


{-@ LIQUID "--real "@-}


[lq| type Valid = {v:Bool | ( (Prop v) <=> true ) } |]

[lq| mulAssoc :: Double -> Double -> Double -> Valid |]
mulAssoc :: Double -> Double -> Double -> Bool
mulAssoc x y z = (x * y) * z == x * (y * z) 

[lq| mulCommut :: Double -> Double -> Valid |]
mulCommut :: Double -> Double -> Bool
mulCommut x y = x * y == y * x 

[lq| mulId :: Double -> Valid |]
mulId :: Double -> Bool
mulId x = x == 1 * x 

[lq| mulDistr :: Double -> Double -> Double -> Valid |]
mulDistr :: Double -> Double -> Double -> Bool
mulDistr x y z = x * (y + z)  == (x * y) + (x * z)

[lq| divId :: Double -> Valid |]
divId :: Double -> Bool
divId x = x / 1.0 == x

[lq| inverse :: {v:Double | v != 0.0} -> Valid |]
inverse :: Double -> Bool
inverse x = 1.0 == x * (1.0 / x)
