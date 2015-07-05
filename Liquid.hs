{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE CPP #-}

{-@ LIQUID "--cabaldir" @-}
{-@ LIQUID "--diff"     @-}

import Language.Haskell.Liquid.CmdLine
import Language.Haskell.Liquid.Driver
import Language.Haskell.Liquid.Types

main :: IO ()
main = do
  cfg0 <- getOpts
  processModules cfg0 (files cfg0)

