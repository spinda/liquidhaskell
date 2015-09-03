
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import qualified Data.Set as S

data T a = T a

[lq| measure elems |]
elems       :: T a -> S.Set a
elems (T a) = S.singleton a

[lq| inline eqelems |]
eqelems :: Eq a => T a -> T a -> Bool
eqelems s t = (elems s) == (elems t)
         
