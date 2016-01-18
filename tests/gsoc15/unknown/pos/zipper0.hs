{- chris
zipper0.hs:18:5:

    "zipper0.hs" (line 19, column 5):
    data Stack a = Stack { focus :: a
        ^
    unexpected reserved word "data"
    expecting variable identifier
-}
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Prelude hiding (reverse)

import Data.Set

data Stack a = Stack { focus  :: !a        -- focused thing in this set
                     , up     :: [a]       -- jokers to the left
                     , down   :: [a] }     -- jokers to the right

[lq| type UListDif a N = {v:[a] | ((not (Set_mem N (listElts v))) && (Set_emp (listDup v)))} |]

[lq|
data Stack a = Stack { focus :: a
                     , up    :: UListDif a focus
                     , down  :: UListDif a focus }
|]

[lq|
  measure listDup :: [a] -> (Set a)
  listDup([]) = {v | Set_emp v }
  listDup(x:xs) = {v | v = if (Set_mem x (listElts xs)) then (Set_cup (Set_sng x) (listDup xs)) else (listDup xs) }
  |]

[lq| type UStack a = {v:Stack a |(Set_emp (Set_cap (listElts (getUp v)) (listElts (getDown v))))}|]

[lq| measure getFocus :: forall a. (Stack a) -> a
    getFocus (Stack focus up down) = focus
  |]

[lq| measure getUp :: forall a. (Stack a) -> [a]
    getUp (Stack focus up down) = up
  |]

[lq| measure getDown :: forall a. (Stack a) -> [a]
    getDown (Stack focus up down) = down
  |]

-- QUALIFIERS
[lq| q :: x:a ->  {v:[a] |(not (Set_mem x (listElts v)))} |]
q :: a -> [a]
q = undefined
[lq| q1 :: x:a ->  {v:[a] |(Set_mem x (listElts v))} |]
q1 :: a -> [a]
q1 = undefined
[lq| q0 :: x:a ->  {v:[a] |(Set_emp(listDup v))} |]
q0 :: a -> [a]
q0 = undefined


[lq| focusUp :: UStack a -> UStack a |]
focusUp :: Stack a -> Stack a
focusUp (Stack t [] rs)     = Stack x xs [] where (x:xs) = reverse (t:rs)
focusUp (Stack t (l:ls) rs) = Stack l ls (t:rs)

[lq| focusDown :: UStack a -> UStack a |]
focusDown :: Stack a -> Stack a
focusDown = reverseStack . focusUp . reverseStack

-- | reverse a stack: up becomes down and down becomes up.
[lq| reverseStack :: UStack a -> UStack a |]
reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls



-- TODO ASSUMES
[lq| reverse :: {v:[a] | (Set_emp (listDup v))} -> {v:[a]|(Set_emp (listDup v))} |]
reverse :: [a] -> [a]
reverse = undefined
