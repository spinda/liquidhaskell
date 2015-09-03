
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude 

[lq| type OList a = [a]<{\fld v -> (v >= fld)}> |]
[lq| gpp :: (Ord a, Monad m) => [a] -> m (OList a) |]
gpp :: (Ord a, Monad m) => [a] -> m [a]
gpp ls = return $ insertSort ls

[lq| gpp' :: (Ord a, Monad m) => [a] -> m (OList a) |]
gpp' :: (Ord a, Monad m) => [a] -> m [a]
gpp' ls 
  = do ls' <- gpp ls
       return ls'

[lq| insertSort :: (Ord a) => xs:[a] -> OList a |]
insertSort            :: (Ord a) => [a] -> [a]
insertSort []         = []
insertSort (x:xs)     = insert x (insertSort xs) 

insert y []                   = [y]
insert y (x : xs) | y <= x    = y : x : xs 
                  | otherwise = x : insert y xs





