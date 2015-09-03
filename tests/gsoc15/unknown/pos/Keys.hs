
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import qualified Data.Set as S

[lq| measure keys |]
keys :: (Ord k) => [(k, v)] -> S.Set k
keys []       = S.empty
keys (kv:kvs) = (S.singleton (myfst kv)) `S.union` (keys kvs)

[lq| measure myfst |]
myfst :: (a, b) -> a
myfst (x, _) = x

-- this is fine

[lq| measure okeys  :: [(a, b)] -> (S.Set a)
    okeys ([])     = (Set_empty 0)
    okeys (kv:kvs) = (Set_cup (Set_sng (fst kv)) (okeys kvs))
  |]
