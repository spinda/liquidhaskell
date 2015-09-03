
{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--totality "@-}
{-# LANGUAGE EmptyDataDecls #-}



import LiquidHaskell


import Prelude hiding (length)

[lq| myhead :: {v:[a] | nonEmpty v} -> a |]
myhead (x:_) = x

[lq| measure nonEmpty |]   
nonEmpty (x:xs) = True 
nonEmpty []     = False

[lq| measure length |]   
length :: [a] -> Int
length (x:xs) = 1 + length xs 
length []     = 0


[lq| measure lenEqFive |]   
lenEqFive (x:xs) = length xs == 4
lenEqFive []     = False

[lq| measure lenNEqFive |]   
lenNEqFive (x:xs) = not (length xs == 4)
lenNEqFive []     = True


[lq| measure lenGEFour |]   
lenGEFour (x:xs) = length xs >= 3
lenGEFour []     = False


[lq| len3 :: {v:[Int] | (not (lenEqFive v))} |]
len3 :: [Int]
len3 = [1, 2, 3]


[lq| len5 :: {v:[Int] | (lenEqFive v) && (lenGEFour v) } |]
len5 :: [Int]
len5 = [1, 2, 3, 4, 5]

[lq| measure length |]

[lq| foo  :: x:[a] -> {v: Bool | (Prop v) <=> (nonEmpty x) } |]
foo  :: [a] -> Bool
foo x = nonEmpty x


cons = (:)
nil  = []








