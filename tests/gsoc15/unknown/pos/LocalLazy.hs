
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| Lazy foo |]
foo x = foo x


bar = liquidAssertB (inf n > 0)
  where n     = choose 0
       [lq| Lazy inf |]
        inf n = inf n
