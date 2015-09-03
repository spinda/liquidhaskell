
{-# LANGUAGE QuasiQuotes #-}


import LiquidHaskell


-- Note: `partialmeasureOld.hs` works fine 

[lq| cons :: x:a -> _ -> {v:[a] | hd v = x} |]
cons x xs = x : xs

[lq| test :: {v:_ | hd v = 0} |]
test :: [Int]
test =  cons 0 [1,2,3,4]

[lq| measure hd |]
hd       :: [a] -> a
hd (x:_) = x


-- Instead of rejecting, can we just default to "un-refined" constructors?

-- Strengthened constructors
--   data [a] where
--     []  :: [a]    -- as before
--     (:) :: x:a -> xs:[a] -> {v:[a] | hd v = x}




