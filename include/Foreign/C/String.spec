module spec Foreign.C.String where

type CStringLen    = ((GHC.Ptr.Ptr Foreign.C.Types.CChar), Nat)<{\p v -> (v <= (plen p))}>
type CStringLenN N = ((GHC.Ptr.Ptr Foreign.C.Types.CChar), {v:Nat | v = N})<{\p v -> (v <= (plen p))}>

measure cStringLen :: Foreign.C.String.CStringLen -> Int
cStringLen (c, n) = n
