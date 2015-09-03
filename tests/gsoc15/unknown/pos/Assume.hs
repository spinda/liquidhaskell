
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| assume foo :: {v:Bool | (Prop v)} |]
foo = False

bar = liquidAssertB foo
