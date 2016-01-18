
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell



-- create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
create l = do
    fp <- mallocByteString l
    return ()

data P a = P a

mallocByteString :: a -> IO (P a)
mallocByteString l = undefined


