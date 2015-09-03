
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


[lq| predicate Lt X Y = X < Y        |]
[lq| predicate Ge X Y = not (Lt X Y) |]
[lq| predicate Pos X  = X > 0        |]

[lq| incr :: x:{v:Int | (Pos v)} -> { v:Int | ((Pos v) && (Ge v x))} |]
incr :: Int -> Int
incr x = x + 1
