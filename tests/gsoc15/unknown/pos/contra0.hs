
{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--no-termination "@-}
{-@ LIQUID "--short-names "@-}



import LiquidHaskell


import Language.Haskell.Liquid.Prelude (liquidAssert)
import Data.IORef


[lq| data variance IO bivariant |]
[lq| data variance IORef bivariant |]

job :: IO () 
job = do
  p <- newIORef (0 :: Int)
  writeIORef p 10
  v <- readIORef p
  liquidAssert (v >= 0) $ return ()
