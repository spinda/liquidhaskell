{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Test7 () where

import LiquidHaskell

[lq| inline gt :: Int -> Int -> Bool |]
gt x y = x > y

[lq| type IntGT X = { v:Int | (gt v) X } |]

thing :: [lq| x:Int |] -> [lq| IntGT {x} |]
thing x = plus x one

plus :: [lq| x:Int |] -> [lq| y:Int |] -> [lq| { v:Int | v = x + y } |]
plus = undefined

one :: [lq| { v:Int | v = 1 } |]
one = undefined

