
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude (liquidError)

infixl 5  `appOL`
infixl 5  `snocOL`
infixr 5  `consOL`

[lq|
data OrdList [olen] a = None
                      | One  (x  :: a)
                      | Many (xs :: ListNE a)
                      | Cons (x  :: a)           (xs :: OrdList a)
                      | Snoc (xs :: OrdList a)   (x  :: a)
                      | Two  (x  :: OrdListNE a) (y  :: OrdListNE a)
|]

[lq|
measure olen :: OrdList a -> Int
olen (None)      = 0
olen (One x)     = 1
olen (Many xs)   = (len xs)
olen (Cons x xs) = 1 + (olen xs)
olen (Snoc xs x) = 1 + (olen xs)
olen (Two x y)   = (olen x) + (olen y)
|]

[lq|
measure olens :: [OrdList a] -> Int
olens ([])     = 0
olens (ol:ols) = (olen ol) + (olens ols)
|]

[lq| type ListNE    a   = {v:[a]       | (len v)  > 0} |]
[lq| type OrdListNE a   = {v:OrdList a | (olen v) > 0} |]
[lq| type OrdListN  a N = {v:OrdList a | (olen v) = N} |]

[lq| invariant {v:OrdList a   | (olen v)  >= 0} |]
[lq| invariant {v:[OrdList a] | (olens v) >= 0} |]


data OrdList a
  = None
  | One a
  | Many [a]          -- Invariant: non-empty
  | Cons a (OrdList a)
  | Snoc (OrdList a) a
  | Two (OrdList a) -- Invariant: non-empty
        (OrdList a) -- Invariant: non-empty


[lq| nilOL    :: OrdListN a {0} |]
[lq| isNilOL  :: xs:OrdList a -> {v:Bool | ((Prop v) <=> ((olen xs) = 0))} |]

[lq| unitOL   :: a              -> OrdListN a {1} |]
[lq| snocOL   :: xs:OrdList a   -> a            -> OrdListN a {1+(olen xs)} |]
[lq| consOL   :: a              -> xs:OrdList a -> OrdListN a {1+(olen xs)} |]
[lq| appOL    :: xs:OrdList a   -> ys:OrdList a -> OrdListN a {(olen xs)+(olen ys)} |]
[lq| concatOL :: xs:[OrdList a] -> OrdListN a {(olens xs)} |]

nilOL        = None
unitOL as    = One as
snocOL as   b    = Snoc as b
consOL a    bs   = Cons a bs
--LIQUID this definition requires `foldr` with abstract refinements, which isn't
--LIQUID in our standard set of specs
-- concatOL aas = foldr appOL None aas
concatOL []       = None
concatOL (ol:ols) = ol `appOL` concatOL ols

--LIQUID as an alternative, we can easily verify the property that, given
--LIQUID non-empty lists, `concatOL` will return a non-empty list
[lq| concatOL' :: ListNE (OrdListNE a) -> OrdListNE a |]
concatOL' []     = liquidError "can't happen"
concatOL' (x:xs) = foldr appOL x xs

isNilOL None = True
isNilOL _    = False

None  `appOL` b     = b
a     `appOL` None  = a
One a `appOL` b     = Cons a b
a     `appOL` One b = Snoc a b
a     `appOL` b     = Two a b

[lq| qualif Go(v:List a, xs:OrdList a, ys:List a): (len v) = (olen xs) + (len ys) |]

[lq| fromOL :: xs:OrdList a -> {v:[a] | (len v) = (olen xs)} |]
fromOL a = go a []
  where
    go None       acc = acc
    go (One a)    acc = a : acc
    go (Cons a b) acc = a : go b acc
    go (Snoc a b) acc = go a (b:acc)
    go (Two a b)  acc = go a (go b acc)
    go (Many xs)  acc = xs ++ acc

[lq| mapOL :: (a -> b) -> xs:OrdList a -> OrdListN b {(olen xs)} |]
mapOL _ None = None
mapOL f (One x) = One (f x)
mapOL f (Cons x xs) = Cons (f x) (mapOL f xs)
mapOL f (Snoc xs x) = Snoc (mapOL f xs) (f x)
mapOL f (Two x y) = Two (mapOL f x) (mapOL f y)
mapOL f (Many xs) = Many (map f xs)

instance Functor OrdList where
  fmap = mapOL

foldrOL :: (a->b->b) -> b -> OrdList a -> b
foldrOL _ z None        = z
foldrOL k z (One x)     = k x z
foldrOL k z (Cons x xs) = k x (foldrOL k z xs)
foldrOL k z (Snoc xs x) = foldrOL k (k x z) xs
foldrOL k z (Two b1 b2) = foldrOL k (foldrOL k z b2) b1
foldrOL k z (Many xs)   = foldr k z xs

foldlOL :: (b->a->b) -> b -> OrdList a -> b
foldlOL _ z None        = z
foldlOL k z (One x)     = k z x
foldlOL k z (Cons x xs) = foldlOL k (k z x) xs
foldlOL k z (Snoc xs x) = k (foldlOL k z xs) x
foldlOL k z (Two b1 b2) = foldlOL k (foldlOL k z b1) b2
foldlOL k z (Many xs)   = foldl k z xs

[lq| toOL :: xs:[a] -> OrdListN a {(len xs)} |]
toOL [] = None
toOL xs = Many xs
