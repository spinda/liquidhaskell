
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| foo :: x:Maybe a -> {v:a | ((isJust(x)) => (fromJust(x) = v)) } |]
foo :: Maybe a -> a 
foo (Just x)  = x 
foo (Nothing) = error "foo"

[lq| bar :: x:Maybe a -> {v:Bool | ((isJust(x)) <=> Prop(v)) } |]
bar (Just x)  = True 
bar (Nothing) = False

