{-# LANGUAGE QuasiQuotes #-}

module Test7 () where

import LiquidHaskell

[lq| inline gt :: Int -> Int -> Bool |]
gt x y = x > y

[lq| type IntGT X = { v:Int | (gt v) X } |]

[lq| thing :: x:Int -> IntGT {x} |]
thing x = 0

