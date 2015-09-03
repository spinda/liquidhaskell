
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data Zoo a = Z { elts :: [a], sz :: Int }

-- | this is not ok (unbound symbol `boo`)
[lq| data Zoo a = Z { elts :: [a], sz :: {v: Int | IsBoo v elts} } |]

[lq| predicate IsBoo V E = V = boo E |]
-- | this is ok

[lq| type Moo a = {v:Zoo a | sz v = boo (elts v)} |]

[lq| inline boo |]
boo    :: [a] -> Int
boo xs = 0
