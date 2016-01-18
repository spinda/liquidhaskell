{- chris
wrap0.hs:14:5:

    "wrap0.hs" (line 14, column 13):
    [lq| assert flibberty :: (Eq a) => a -> Bool |]
                ^
    unexpected "f"
    expecting "::"
-}
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude (liquidError, liquidAssertB)

data Foo a = F a

type IntFoo = Foo Int

[lq| assert flibberty :: (Eq a) => a -> Bool |]
flibberty x   = prop x (F x)
prop x (F y)  = liquidAssertB (x == y)

[lq| assert flibInt :: (Num a, Ord a) => a -> Bool |]
flibInt x     = prop1 x (F (x + 1))
prop1 x (F y) = liquidAssertB (x < y)

[lq| assert flibXs :: a -> Bool |]
flibXs x     = prop2 (F [x, x, x])
prop2 (F []) = liquidError "no!"
prop2 (F _ ) = True
