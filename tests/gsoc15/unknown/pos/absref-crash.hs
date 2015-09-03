
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data L a = C (L a)

[lq| data L a <p :: L a -> Prop> = C { xs :: L<p> a } |]

[lq| Lazy foo |]
foo :: b -> L a
foo x = C $ foo x
