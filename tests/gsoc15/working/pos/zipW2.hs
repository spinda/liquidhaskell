
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell

main = undefined -- Chris: had to add main to make it typecheck

foo :: [Int] -> [Int]
foo zs = zipWith (+) zs zs
