
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


{-@ LIQUID "--totality "@-}
import Prelude hiding (fromJust, tail, head)

[lq| fromJust :: {v:Maybe a | (isJust v)} -> a |]
fromJust :: Maybe a -> a
fromJust (Just a) = a

[lq| tail :: {v:[a] | ((len v) > 0)}-> [a] |]
tail :: [a] -> [a]
tail (x:xs) = xs

[lq| head :: {v:[a] | ((len v) > 0)}-> a |]
head :: [a] -> a
head (x:xs) = x


-- USERS

gotail xs = case xs of
             [] -> []
             y : ys -> tail xs

[lq| gohead :: [{v:[a] | ((len v) > 0)}] -> [a] |]
gohead :: [[a]] -> [a]
gohead xs = map head xs 
