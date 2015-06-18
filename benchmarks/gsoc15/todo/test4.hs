{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test4 () where

import LiquidHaskell

assert :: [lq| { x:Bool | x == True } -> a -> a |]
assert _ = id

useless = assert False

