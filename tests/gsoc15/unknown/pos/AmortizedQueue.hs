
{-# LANGUAGE QuasiQuotes #-}
{-@ LIQUID "--no-termination "@-}
{-@ LIQUID "--maxparams=3 "@-}



import LiquidHaskell


-- Source: Okasaki, JFP 1995
-- http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf

--------------------------------------------------------------------------------
-- | Sized Lists
--------------------------------------------------------------------------------

data SList a = SL { size  :: Int
                  , elems :: [a]
                  }

[lq| type SListN a N = {v:SList a | size v = N} |]

-- | Invariant: `size` is really the size:

[lq| data SList a = SL { size  :: Int
                      , elems :: {v:[a] | len v = size}
                      }
  |]

-- | Size function actually returns the size: (Duh!)

[lq| size :: q:SList a -> {v:Nat | v = size q} |]

-- | Non-Empty Lists:

[lq| type NEList a = {v:SList a | size v > 0} |]


[lq| nil          :: SListN a 0  |]
nil              = SL 0 []

[lq| cons         :: a -> xs:SList a -> SListN a {size xs + 1}   |]
cons x (SL n xs) = SL (n+1) (x:xs)

[lq| tl           :: xs:NEList a -> SListN a {size xs - 1}  |]
tl (SL n (_:xs)) = SL (n-1) xs
tl _             = die "never"

[lq| hd           :: xs:NEList a -> a |]
hd (SL _ (x:_))  = x 
hd _             = die "never"


--------------------------------------------------------------------------------
-- | Sized Lists
--------------------------------------------------------------------------------

data Queue a = Q  { left   :: SList a
                  , right  :: SList a
                  }

-- | Invariant: `|right|` <= `|left|`

[lq| data Queue a = Q { left  :: SList a 
                     , right :: {v:SList a | size v <= size left}
                     }
  |]



emp = Q nil nil

qsize         :: Queue a -> Int
qsize (Q l r) = size l + size r

insert e (Q l r) = makeq l (e `cons` r)

[lq| makeq :: l:_ -> r:{ _ | size r <= size l + 1} -> _  |]
makeq l r
  | size r <= size l = Q l r
  | otherwise        = Q (rot l r nil) nil

[lq| rot :: l:_ -> r:SListN _ {1 + size l} -> a:_ -> {v:_ | size v = size l + size r + size a} |]
rot l r a
  | size l == 0      = (hd r) `cons` a
  | otherwise        = (hd l) `cons` (rot (tl l) (tl r) ((hd r) `cons` a))

[lq| die :: {v:_ | false} -> a |]
die x = error x
