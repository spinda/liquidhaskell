module Language.Haskell.Liquid.Bare (
    GhcSpec (..)
  , makeGhcSpec
  ) where

import GHC

import CoreSyn
import HscTypes
import NameSet
import Var

import Data.Monoid

import Language.Haskell.Liquid.GhcMisc ()
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.Measure as Ms

makeGhcSpec :: Config -> ModName -> [CoreBind] -> [Var] -> [Var] -> NameSet -> HscEnv -> Either Error LogicMap
            -> [(ModName, Ms.BareSpec)]
            -> IO GhcSpec
makeGhcSpec cfg _name _cbs _vars _defVars _exports _env _lmap _spec =
  return $ emptySpec cfg

emptySpec :: Config -> GhcSpec
emptySpec cfg =
  SP [] [] [] [] [] [] [] [] [] mempty [] [] [] [] mempty mempty mempty cfg mempty [] mempty mempty

