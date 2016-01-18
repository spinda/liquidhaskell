
{-# LANGUAGE QuasiQuotes #-}
{- Example of AVL trees by michaelbeaumont -}

{-@ LIQUID "--no-termination "@-}
{-@ LIQUID "--totality "@-}



import LiquidHaskell


-- Basic functions
data Tree a = Nil | Tree { key :: a, l::Tree a, r :: Tree a} deriving Show

[lq| data Tree [ht] a = Nil
                     | Tree { key :: a
                            , l   :: Tree {v:a | v < key }
                            , r   :: Tree {v:a | key < v }
                            }
  |]

[lq| measure ht |]
ht              :: Tree a -> Int
ht Nil          = 0
ht (Tree _ l r) = if (ht l) > (ht r) then (1 + ht l) else (1 + ht r)
[lq| invariant {v:Tree a | 0 <= ht v} |]


[lq| measure bFac |]
[lq| bFac :: t:AVLTree a -> {v:Int | v = bFac t && 0 <= v + 1 && v <= 1} |]
bFac Nil          = 0
bFac (Tree _ l r) = ht l - ht r

[lq| htDiff :: s:Tree a -> t: Tree a -> {v: Int | HtDiff s t v} |]
htDiff :: Tree a -> Tree a -> Int
htDiff l r = ht l - ht r

-- | Empty
[lq| empty :: {v: AVLTree a | ht v == 0} |]
empty = Nil

-- | Singleton
[lq| singleton :: a -> {v: AVLTree a | ht v == 1 }|]
singleton a = Tree a Nil Nil

-- | Insert

[lq| predicate PostInsert S T = ((bFac T == 0) => (EqHt T S || ht T == 1)) && (EqHt T S || HtDiff T S 1) |]

[lq| insert :: a -> s: AVLTree a -> {t: AVLTree a | PostInsert s t } |]
insert :: (Ord a) => a -> Tree a -> Tree a
insert a Nil = singleton a
insert a t@(Tree v _ _) = case compare a v of
    LT -> insL a t
    GT -> insR a t
    EQ -> t

[lq| insL :: x:a -> s:{AVLTree a | x < key s && ht s > 0} -> {t: AVLTree a | PostInsert s t } |]
insL a (Tree v l r)
  | siblDiff == 2 && bl' == 1  = rebalanceLL v l' r
  | siblDiff == 2 && bl' == -1 = rebalanceLR v l' r
  | siblDiff <= 1              = Tree v l' r
  where
    l'                       = insert a l
    siblDiff                 = htDiff l' r
    bl'                      = bFac l'

[lq| insR :: x:a -> s:{AVLTree a | key s < x && ht s > 0} -> {t: AVLTree a | PostInsert s t } |]
insR a (Tree v l r)
  | siblDiff == 2 && br' == 1  = rebalanceRL v l r'
  | siblDiff == 2 && br' == -1  = rebalanceRR v l r'
  | siblDiff <= 1            = Tree v l r'
  where
    siblDiff                 = htDiff r' l
    r'                       = insert a r
    br'                      = bFac r'


[lq| rebalanceLL :: x:a -> l:{AVLL a x | LeftHeavy l } -> r:{AVLR a x | HtDiff l r 2} -> {t:AVLTree a | EqHt t l} |]
rebalanceLL v (Tree lv ll lr) r                 = Tree lv ll (Tree v lr r)

[lq| rebalanceLR :: x:a -> l:{AVLL a x | RightHeavy l } -> r:{AVLR a x | HtDiff l r 2} -> {t: AVLTree a | EqHt t l } |]
rebalanceLR v (Tree lv ll (Tree lrv lrl lrr)) r = Tree lrv (Tree lv ll lrl) (Tree v lrr r)

[lq| rebalanceRR :: x:a -> l: AVLL a x -> r:{AVLR a x | RightHeavy r && HtDiff r l 2 } -> {t: AVLTree a | EqHt t r } |]
rebalanceRR v l (Tree rv rl rr)                 = Tree rv (Tree v l rl) rr

[lq| rebalanceRL :: x:a -> l: AVLL a x -> r:{AVLR a x | LeftHeavy r && HtDiff r l 2} -> {t: AVLTree a | EqHt t r } |]
rebalanceRL v l (Tree rv (Tree rlv rll rlr) rr) = Tree rlv (Tree v l rll) (Tree rv rlr rr)

-- Test
main = do
    mapM_ print [a,b,c,d]
  where
    a = singleton 5
    b = insert 2 a
    c = insert 3 b
    d = insert 7 c

-- Liquid Haskell

[lq| predicate HtDiff S T D = ht S - ht T == D |]
[lq| predicate EqHt S T     = HtDiff S T 0     |]
[lq| predicate LeftHeavy  T = bFac T == 1      |]
[lq| predicate RightHeavy T = bFac T == -1     |]

[lq| measure balanced |]
balanced              :: Tree a -> Bool
balanced (Nil)        = True
balanced (Tree v l r) = ht l - ht r <= 1 && ht l - ht r >= -1 && balanced l && balanced r

[lq| type AVLTree a   = {v: Tree a | balanced v} |]
[lq| type AVLL a X    = AVLTree {v:a | v < X}    |]
[lq| type AVLR a X    = AVLTree {v:a | X < v}    |]
