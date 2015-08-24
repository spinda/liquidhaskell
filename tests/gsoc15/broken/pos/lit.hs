-- TODO: class/instance measures
-- TODO: add `strLen` to the fixpoint environment

{-# LANGUAGE QuasiQuotes #-}

module Lit where

import LiquidHaskell
import LiquidHaskell.Prelude
import Prelude hiding (length)

[lq| test :: {v:Int | v == 3} |]
test = length "cat"

[lq| length :: xs:[a] -> {v:Int | v = len xs}|]
length []     = 0
length (_:xs) = 1 + length xs
