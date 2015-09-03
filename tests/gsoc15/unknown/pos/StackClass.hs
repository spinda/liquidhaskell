
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import qualified Data.Set

data Stack a = S [a]

data Foo a = F {stack :: Stack a}

[lq| bar :: xs:[Foo a] -> {v:[Foo a] |(eltss v) = (eltss xs)} |]
bar :: [Foo a] -> [Foo a]
bar = (F (S []):)

foo = F 
[lq| class measure elts  :: forall f a. f a -> Data.Set.Set a |]
[lq| class measure eltss  :: forall f a. [f a] -> Data.Set.Set a |]

[lq| instance measure elts :: Stack a -> (Data.Set.Set a)
    elts (S xs) = (listElts xs)
  |]

[lq| instance measure elts :: Foo a -> (Data.Set.Set a)
    elts (F st) = (elts st)
  |]

[lq| instance measure  eltss :: [(Foo a)] -> (Data.Set.Set a)
    eltss([]) = {v| Set_emp v }
    eltss(x:xs) = (Set_cup (elts x) (eltss xs))
  |]
