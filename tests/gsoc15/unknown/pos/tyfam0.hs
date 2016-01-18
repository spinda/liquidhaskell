
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Control.Monad.Primitive

import Data.Vector.Generic.Mutable

[lq| copyOffset :: (PrimMonad m, MVector v e)
           => v (PrimState m) e -> v (PrimState m) e -> Int -> Int -> Int -> m ()
  |]

copyOffset :: (PrimMonad m, MVector v e)
           => v (PrimState m) e -> v (PrimState m) e -> Int -> Int -> Int -> m ()
copyOffset = undefined

[lq| zog :: (m s a) -> Nat |]
zog :: (m s a) -> Int
zog = undefined
