
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


data Map k a  = Tip

[lq| data Map k a <l :: root:k -> k -> Prop>
         = Tip
  |]

[lq| measure isBin :: Map k a -> Prop
    isBin (Tip)          = false
  |]

trim :: Map k a
trim = error "TODO"

