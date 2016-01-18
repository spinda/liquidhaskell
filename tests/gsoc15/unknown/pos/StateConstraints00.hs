
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Prelude hiding (Monad, return )

-- | TODO 
-- | 
-- | 1. default methods are currently not supported

data ST s a = ST {runState :: s -> (a,s)}

[lq| data ST s a <r :: a -> Prop> 
  = ST (runState :: x:s -> (a<r>, s)) |]

[lq| runState :: forall <r :: a -> Prop>. ST <r> s a -> x:s -> (a<r>, s) |]


class Foo m where
  return :: a -> m a


instance Foo (ST s) where
  [lq| instance Foo ST s where
    return :: forall s a. x:a -> ST <{\v -> x == v}> s a
    |]
  return x     = ST $ \s -> (x, s)
 

[lq| foo :: w:a -> ST <{v:a | v = w}>  Bool a |]
foo :: a -> ST Bool a
foo x = return x


bar = runState (foo 0) True














