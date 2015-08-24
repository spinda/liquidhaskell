{-

Specification for (.) needs abs refinements

-}

{-# LANGUAGE QuasiQuotes #-}

module Goo (
    module Moo 
  , plusTwo
 ) where

import LiquidHaskell

import Moo 

[lq| plusTwo :: x:Int -> { v:Int | v = x + 2 } |]
plusTwo = plusOne . plusOne

