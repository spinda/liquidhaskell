
{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--no-termination "@-}
{-@ LIQUID "--short-names "@-}



import LiquidHaskell


import Language.Haskell.Liquid.Prelude (liquidAssert)
import Data.IORef


data Foo a b c d

[lq| data variance IO bivariant |]

foo :: IO ()
foo = do a <- return 0 
         liquidAssert (a == 0) (return ())

foo' :: IO ()
foo' = bind (return 0) (\a -> liquidAssert (a == 0) (return ()))

[lq| data variance IORef bivariant |]

[lq| data variance Foo invariant bivariant covariant contravariant |]

[lq| job' :: IORef {v:Int |  v = 4} -> IO () |]
job' :: IORef Int -> IO ()
job' p = 
	bind (readIORef p) (\v -> liquidAssert (v == 4) (return ()))


[lq| bind :: Monad m => m a -> (a -> m b) -> m b |]
bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)























