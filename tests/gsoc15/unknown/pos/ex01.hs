
{-# LANGUAGE QuasiQuotes #-}

-- | A somewhat fancier example demonstrating the use of Abstract Predicates and exist-types



import LiquidHaskell



-------------------------------------------------------------------------
-- | Data types ---------------------------------------------------------
-------------------------------------------------------------------------

data Vec a = Nil 

[lq| efoldr :: forall b a <p :: x0:Vec a -> x1:b -> Prop>. 
              b <p Ex.Nil>
              -> ys: Vec a
              -> b <p ys>
  |]
efoldr :: b -> Vec a -> b
efoldr b Nil         = b

