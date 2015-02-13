module spec Prelude where


GHC.Exts.D# :: x:_ -> {v:_ | v = x}

assume GHC.Base..               :: forall< p :: xx:b -> c -> Prop
                                         , q :: yy:a -> b -> Prop>.
                                      f:(x:b -> c<p x>) ->
                                      g:(y:a -> b<q y>) ->
                                      x:a ->
                                      exists[z:b<q x>].c<p z>
assume GHC.Integer.smallInteger :: x:GHC.Prim.Int#
                                -> { v:GHC.Integer.Type.Integer |
                                     v = (x :: int) }
assume GHC.Num.+                :: (GHC.Num.Num a) => x:a -> y:a -> {v:a | v = x + y }
assume GHC.Num.-                :: (GHC.Num.Num a) => x:a -> y:a -> {v:a | v = x - y }

embed GHC.Types.Double as real
embed GHC.Integer.Type.Integer  as int

type IncrListD a D = [a]<{\x y -> (x+D) <= y}>
