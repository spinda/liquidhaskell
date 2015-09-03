
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell

-- TODO: get this working with the ! annots.

import Language.Haskell.Liquid.Prelude
-- remove the ! and it is safe...
data MaybeS a = NothingS | JustS !a
-- (SAFE) data MaybeS a = NothingS | JustS a

[lq| measure isJustS :: forall a. MaybeS a -> Prop 
    isJustS (JustS x)  = true
    isJustS (NothingS) = false
  |]

[lq| measure fromJustS :: forall a. MaybeS a -> a
    fromJustS (JustS x) = x 
  |]

gloop = poop True

[lq| poop :: z:a -> {v: MaybeS a | fromJustS(v) = z} |]
poop z = JustS z



























