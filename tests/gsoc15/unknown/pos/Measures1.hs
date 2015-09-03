
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| data Wrapper a <p :: a -> Prop, r :: a -> a -> Prop > 
    = Wrap (rgref_ref :: a<p>) |]
data Wrapper a = Wrap (a)

-- Two measures
[lq| measure fwdextends :: Int -> Int -> Prop |]
[lq| measure actionP :: Int -> Prop |]

{- data Wrapper2  = Wrapper2 (unwrapper :: (Wrapper<{\x -> (true)},{\x y -> (fwdextends y x)}> Int )) @-}
[lq| data Wrapper2  = Wrapper2 (unwrapper :: (Wrapper<{\x -> (actionP x)},{\x y -> (true)}> Int )) |]
data Wrapper2  = Wrapper2 (Wrapper (Int) )


