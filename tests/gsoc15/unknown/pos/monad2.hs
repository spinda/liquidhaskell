
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


goo c = return c

foo = 
  do x <- Just 1
     y <- goo 3
     return $ x + y 
