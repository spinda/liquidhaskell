{-

Chris:

zipW.hs:10:5:

    "zipW.hs" (line 10, column 13):
    [lq| assert zipW :: (a -> b -> c) -> xs : [a] -> ys:{v:[b] | len(v) = len(xs)} -> {v : [c] | len(v) = len(xs)} |]
                ^
    unexpected "z"
    expecting "::"

-}

{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

[lq| assert zipW :: (a -> b -> c) -> xs : [a] -> ys:{v:[b] | len(v) = len(xs)} -> {v : [c] | len(v) = len(xs)} |]
zipW :: (a->b->c) -> [a]->[b]->[c]
zipW f (a:as) (b:bs) = f a b : zipW f as bs
zipW _ [] []         = []
zipW _ [] (_:_)      = liquidError "zipWith1"
zipW _ (_:_) []      = liquidError "zipWith1"

[lq| assert foo :: (a -> b -> c) -> xs : [a] -> ys:{v:[b] | len(v) = len(xs)} -> {v : [c] | len(v) = len(xs)} |]
foo = zipW
