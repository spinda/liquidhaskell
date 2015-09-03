
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import qualified ResolveB as RB

[lq| measure getFooA :: Foo -> Int
    getFooA (Foo x) = x
  |]

data Foo = Foo Int

y = RB.Foo 1

[lq| qualif NotA(v:RB.Bar): (notA v) |]

[lq| measure notA :: RB.Bar -> Prop
    notA (RB.A) = false
    notA (RB.B) = true
    notA (RB.C) = false
  |]

[lq| predicate NotA V = V != RB.A |]
