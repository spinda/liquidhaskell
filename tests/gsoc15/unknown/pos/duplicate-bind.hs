
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Language.Haskell.Liquid.Prelude

insert key value [] = [(key, value)]
insert _ _ _        = error ""
