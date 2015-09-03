
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data Bob = B {foo :: Int}
[lq| data Bob = B {foo :: Int} |]

[lq| foo :: x:Bob -> {v:Int | v = foo x} |]

[lq| invariant {v:Bob | foo v == 10} |]

mk :: a -> Bob
mk = undefined

[lq| propFAIL :: {v:_ | foo v = 10} |]
propFAIL = mk ()

[lq| propOK :: {v:_ | foo v = 10} |]
propOK = let z = mk () in z 


