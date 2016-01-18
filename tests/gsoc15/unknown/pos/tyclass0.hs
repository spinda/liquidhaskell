
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


class Zog a where
  zoom :: a -> Int

-- Assume the relevant behavior for the method.
[lq| zoom :: (Zog a) => a -> Nat |]

-- Uses the behavior of `zoom`
[lq| poop :: (Zog a) => a -> Nat |]
poop x = zoom x
