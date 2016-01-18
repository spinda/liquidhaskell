
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

x     = choose 0

baz y = y

prop  = liquidAssertB (baz True)
