module Range where

import Language.Haskell.Liquid.Prelude

goo x = let z = [x] in z

z0 _  = True
z1 _  = False

poo (x:_) = 0 == 0 
poo ([])  = assert False

xs = goo (choose 0)

prop1 = assert (poo xs)
