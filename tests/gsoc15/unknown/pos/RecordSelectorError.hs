
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


{-@ LIQUID "--totality "@-}


data F a b = F {fx :: a, fy :: b} | G {fx :: a}
[lq| data F a b = F {fx :: a, fy :: b} | G {fx :: a} |]

[lq| measure isF :: F a b -> Prop
    isF (F x y) = true
    isF (G x)   = false
  |]

-- Record's selector type is defaulted to true as imported
[lq| fy  :: {v:F a b | (isF v)} -> b |]
[lq| bar :: {v:F a b | (isF v)} -> b |]
bar :: F a b  -> b
bar = fy
