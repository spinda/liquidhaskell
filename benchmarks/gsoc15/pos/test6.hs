{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test6 () where

import LiquidHaskell

data Wow = Yes | No

no :: [lq| { v:Wow | v == No } |]
no = No

