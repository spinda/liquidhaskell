
{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--no-termination "@-}



import LiquidHaskell


---------------------------------------------------------------------
----------------------- Datatype Definition -------------------------
---------------------------------------------------------------------

data Bndr 

data Expr 
  = Lam Bndr Expr
  | Var Bndr  
  | App Expr Expr

[lq|
data Expr [elen] 
  = Lam (x::Bndr) (e::Expr)
  | Var (x::Bndr)  
  | App (e1::Expr) (e2::Expr)
|]

[lq| measure elen :: Expr -> Int
    elen(Var x)     = 0
    elen(Lam x e)   = 1 + (elen e) 
    elen(App e1 e2) = 1 + (elen e1) + (elen e2) 
  |]

[lq| invariant {v:Expr | (elen v) >= 0} |]

[lq|  measure isValue :: Expr -> Prop
     isValue (Lam x e)    = true 
     isValue (Var x)      = false
     isValue (App e1 e2)  = false
  |]

[lq| type Value = {v: Expr | isValue v } |]
[lq| type Store = [(Bndr, Value)]            |]

---------------------------------------------------------------------
-------------------------- The Evaluator ----------------------------
---------------------------------------------------------------------

[lq| evalVar :: Bndr -> Store -> Value |]
evalVar :: Bndr -> [(Bndr, Expr)] -> Expr 
evalVar = error "HIDEME"

[lq| Decrease eval 2 |]

[lq| eval :: sto:Store -> e:Expr -> (Store, Value) |]

eval sto (Var x)  
  = (sto, evalVar x sto)

eval sto (App e1 e2)
  = let (_,    v2 ) = eval sto e2 
        (sto1, e1') = eval sto e1
    in case e1' of
         (Lam x e) -> eval ((x, v2) : sto1) e
         _         -> error "non-function application"

eval sto (Lam x e) 
  = (sto, Lam x e)

