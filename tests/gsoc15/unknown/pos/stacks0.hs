
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Data.Set (Set(..)) 

data LL a = Nil | Cons { head :: a, tail :: LL a }

[lq| data LL a = Nil | Cons { head :: a 
                           , tail :: {v: LL a | not (Set_mem head (elts v))  } } 
  |]

[lq| measure elts :: LL a -> (Set a) 
    elts (Nil)       = {v | (Set_emp v)}
    elts (Cons x xs) = {v | v = (Set_cup (Set_sng x) (elts xs)) }
  |]

[lq| predicate Disjoint X Y = (Set_emp (Set_cap (elts X) (elts Y))) |]  
[lq| predicate NotIn    X Y = not (Set_mem X (elts Y))              |] 

ll2 = Cons x0 (Cons x1 (Cons x2 Nil))
  where x0 :: Int 
        x0  = 0
        x1  = 1
        x2  = 2

[lq| data Stack a = St { focus  :: a    
                      , up     :: {vu: LL a | (NotIn focus vu) } 
                      , down   :: {vd: LL a | ((NotIn focus vd) && (Disjoint up vd)) } 
                      } 
  |]

data Stack a = St { focus  :: !a    
                  , up     :: !(LL a) 
                  , down   :: !(LL a)
                  } 

[lq| fresh :: a -> Stack a |]
fresh x = St x Nil Nil

[lq| moveUp :: Stack a -> Stack a |]
moveUp (St x (Cons y ys) zs) = St y ys (Cons x zs)
moveUp s                     = s 

[lq| moveDn :: Stack a -> Stack a |]
moveDn (St x ys (Cons z zs)) = St z (Cons x ys) zs
moveDn s                     = s 



