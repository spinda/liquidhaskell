{-# LANGUAGE QuasiQuotes #-}

module Test8 () where

import LiquidHaskell

[lq| inline gt :: Int -> Int -> Bool |]
gt x y = x > y

[lq| type IntGT X = { v:Int | gt v X 7 } |]

