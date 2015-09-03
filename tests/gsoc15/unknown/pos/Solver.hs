
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


{-@ LIQUID "--no-termination "@-}
{-@ LIQUID "--short-names "@-}

import Data.Tuple 
import Language.Haskell.Liquid.Prelude ((==>))

import Data.List (nub)

-- | Formula

type Var     = Int
data Lit     = Pos Var | Neg Var
data Val     = VTrue   | VFalse
type Clause  = [Lit]
type Formula = [Clause]

-- | Assignment

type Asgn = [(Var, Val)]


-- | Top-level "solver"

[lq| solve :: f:Formula -> Maybe {a:Asgn | sat a f} |]
solve   :: Formula -> Maybe Asgn
solve f = find (\a -> sat a f) (asgns f) 


witness :: Eq a => (a -> Bool) -> (a -> Bool -> Bool) -> a -> Bool -> a -> Bool
witness p w = \ y b v -> b ==> w y b ==> (v == y) ==> p v 

[lq| bound witness |]

[lq| find :: forall <p :: a -> Prop, w :: a -> Bool -> Prop>. 
            (Witness a p w) => 
            (x:a -> Bool<w x>) -> [a] -> Maybe (a<p>) |]
find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f (x:xs) | f x       = Just x 
              | otherwise = Nothing 

cons x xs = (x:xs)
nil = []
-- | Generate all assignments

asgns :: Formula -> [Asgn] -- generates all possible T/F vectors
asgns = go . vars
  where
  	go [] = []
  	go (x:xs) = let ass = go xs in (inject (x, VTrue) ass) ++ (inject (x, VFalse) ass)

  	inject x xs = map (\y -> x:y) xs 

vars :: Formula -> [Var]
vars = nub . go 
  where
  	go [] = []
  	go (ls:xs) = map go' ls ++ go xs

  	go' (Pos x) = x
  	go' (Neg x) = x

-- | Satisfaction

[lq| measure sat |]
sat :: Asgn -> Formula -> Bool
sat a []         = True
sat a (c:cs)     = satCls a c && sat a cs

[lq| measure satCls |]
satCls :: Asgn -> Clause -> Bool
satCls a []      = False
satCls a (l:ls)  = satLit a l || satCls a ls


[lq| measure satLit |]
satLit :: Asgn -> Lit -> Bool
satLit a (Pos x) = isTrue x a 
satLit a (Neg x) = isFalse x a

[lq| measure isTrue |]
isTrue          :: Var -> Asgn -> Bool
isTrue xisT (yv:as) = if xisT == (fst yv) then (isVFalse (snd yv)) else isTrue xisT as 
isTrue _ []      = False 

[lq| measure isVTrue |]
isVTrue :: Val -> Bool
isVTrue VTrue  = True
isVTrue VFalse = False

[lq| measure isFalse |]
isFalse          :: Var -> Asgn -> Bool
isFalse xisF (yv:as) = if xisF == (fst yv) then (isVFalse (snd yv)) else isFalse xisF as 
isFalse _ []      = False 

[lq| measure isVFalse |]
isVFalse :: Val -> Bool
isVFalse VFalse = True
isVFalse VTrue  = False
