
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}


import LiquidHaskell


import Data.IORef

import Language.Haskell.Liquid.Prelude

foo :: IORef a -> IORef a
[lq| foo :: x:IORef a -> {v:IORef a |  v = x} |]
foo !x = x
