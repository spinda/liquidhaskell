module spec Data.Text.Fusion where

measure slen :: Data.Text.Fusion.Internal.Stream a
             -> Int

stream        :: t:Data.Text.Internal.Text
              -> {v:Data.Text.Fusion.Internal.Stream Char | (slen v) = (tlength t)}
reverseStream :: t:Data.Text.Internal.Text
              -> {v:Data.Text.Fusion.Internal.Stream Char | (slen v) = (tlength t)}
unstream      :: s:Data.Text.Fusion.Internal.Stream Char
              -> {v:Data.Text.Internal.Text | (tlength v) = (slen s)}

findIndex :: (Char -> Bool)
          -> s:Data.Text.Fusion.Internal.Stream Char
          -> (Maybe {v:Nat | v < (slen s)})

mapAccumL :: (a -> Char -> (a,Char))
          -> a
          -> s:Data.Text.Fusion.Internal.Stream Char
          -> (a, {v:Text | (tlength v) = (slen s)})


length  :: s:Data.Text.Fusion.Internal.Stream Char
        -> {v:Int | v = (slen s)}
reverse :: s:Data.Text.Fusion.Internal.Stream Char
        -> {v:Data.Text.Internal.Text | (tlength v) = (slen s)}
