{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test5 () where

import LiquidHaskell

[lq| inline lt |]
lt :: Int -> Int -> Bool
lt x y = x < y

[lq| inline gt :: Int -> Int -> Bool |]
gt x y = x > y

