-- chris: done't like "assert"
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude (safeZipWith)

[lq| assert foo :: (a -> b -> c) -> xs : [a] -> ys:{v:[b] | len(v) = len(xs)} -> {v : [c] | len(v) = len(xs)} |]
foo = safeZipWith
