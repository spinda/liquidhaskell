
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-@ LIQUID "--c-files=../ffi-include/foo.c "@-}
[lq| LIQUID "-i../ffi-include" |]


import LiquidHaskell


import Foreign.C.Types

[lq| embed CInt as int |]
[lq| embed Integer as int |]

[lq| assume c_foo :: x:{CInt | x > 0} -> IO {v:CInt | v = x} |]
foreign import ccall unsafe "foo.c foo" c_foo
  :: CInt -> IO CInt

main :: IO ()
main = print . fromIntegral =<< c_foo 1
