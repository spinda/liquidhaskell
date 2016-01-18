
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-@ LIQUID "--no-termination "@-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude
import Prelude hiding (sum, length, (!!), Functor(..))
import qualified Prelude as P

[lq| qualif Size(v:int, xs:a): v = (size xs) |]

[lq| data List a = Nil | Cons (hd::a) (tl::(List a)) |]
data List a = Nil | Cons a (List a)

[lq| length :: xs:List a -> {v:Nat | v = (size xs)} |]
length :: List a -> Int
length Nil         = 0
length (Cons x xs) = 1 + length xs

[lq| (!!) :: xs:List a -> {v:Nat | v < (size xs)} -> a |]
(!!) :: List a -> Int -> a
Nil         !! i = liquidError "impossible"
(Cons x _)  !! 0 = x
(Cons x xs) !! i = xs !! (i - 1)

[lq| class measure size :: forall a. a -> Int |]
[lq| class Sized s where
      size :: forall a. x:s a -> {v:Nat | v = (size x)}
  |]
class Sized s where
  size :: s a -> Int

instance Sized List where
  [lq| instance measure size :: List a -> Int
      size (Nil)       = 0
      size (Cons x xs) = 1 + (size xs)
    |]
  size = length

instance Sized [] where
  [lq| instance measure size :: [a] -> Int
      size ([])   = 0
      size (x:xs) = 1 + (size xs)
    |]
  size [] = 0
  size (x:xs) = 1 + size xs

[lq| class (Sized s) => Indexable s where
      index :: forall a. x:s a -> {v:Nat | v < (size x)} -> a
  |]
class (Sized s) => Indexable s where
  index :: s a -> Int -> a


instance Indexable List where
  index = (!!)

[lq| sum :: Indexable s => s Int -> Int |]
sum :: Indexable s => s Int -> Int
sum xs = go max 0
  where
    max = size xs
    go (d::Int) i
      | i < max   = index xs i + go (d-1) (i+1)
      | otherwise = 0


[lq| sumList :: List Int -> Int |]
sumList :: List Int -> Int
sumList xs = go max 0
  where
    max = size xs
    go (d::Int) i
      | i < max   = index xs i + go (d-1) (i+1)
      | otherwise = 0


[lq| x :: {v:List Int | (size v) = 3}  |]
x :: List Int
x = 1 `Cons` (2 `Cons` (3 `Cons` Nil))

foo = liquidAssert $ size (Cons 1 Nil) == size [1]
