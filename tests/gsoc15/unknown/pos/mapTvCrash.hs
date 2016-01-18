
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


import Foreign.ForeignPtr

[lq| foo :: FinalizerPtr a -> a |]
foo :: FinalizerPtr a -> a
foo = undefined
