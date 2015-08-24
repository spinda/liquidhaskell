{-# LANGUAGE QuasiQuotes #-}

module Test6 () where

import LiquidHaskell

data Wow = Yes | No

[lq| no :: { v:Wow | v == No } |]
no = No

