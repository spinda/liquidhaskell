{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test7 () where

import LiquidHaskell

[lq| embed Int as int |]

[lq| inline gt :: Int -> Int -> Bool |]
gt x y = x > y

[lq| type IntGT X = { v:Int | (gt v) X } |]

thing :: [lq| x:Int |] -> [lq| IntGT {x} |]
thing x = 0

