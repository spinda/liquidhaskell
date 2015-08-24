module Language.Haskell.Liquid.Spec.Lookup (
    -- * Lookup Named Things in Ghc
    lookupGhcTyCon
  , lookupGhcDataCon
  , lookupGhcVar

    -- * Optional Lookup Combinator
  , tryLookup
  ) where

import GHC hiding (Located)

import DataCon
import Exception
import GhcMonad
import HscTypes
import MonadUtils
import TcRnMonad
import TcSplice
import TyCon
import Var

import Data.Maybe

import qualified Language.Haskell.TH.Syntax as TH

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Errors ()
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.Types

--------------------------------------------------------------------------------
-- Lookup Named Things in Ghc --------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Rewrite the SrcSpan attached to SourceErrors from parseName so that
--       they point to the correct location (currently they show as coming from
--       somewhere in "interactive"

class GhcLookup n where
  toNames :: n -> Ghc [Name]

instance GhcLookup Symbol where
  toNames = parseName . symbolString

instance GhcLookup Name where
  toNames = return . return

instance GhcLookup TH.Name where
  toNames = parseName . show


lookupGhcThing :: (GhcLookup n, PPrint n) => String -> (TyThing -> Maybe a) -> Located n -> Ghc a
lookupGhcThing desc ofThing ident = do
  names    <- toNames $ val  ident
  things   <- mapMaybeM lookupName names
  let thing = listToMaybe $ mapMaybe ofThing things
  case thing of
    Just x  -> return x
    Nothing -> throwGhc err
  where
    err :: Error
    err = ErrNotInScope (locatedSrcSpan ident) (text desc) (pprint $ val ident)


lookupGhcTyCon :: (GhcLookup n, PPrint n) => Located n -> Ghc TyCon
lookupGhcTyCon = lookupGhcThing "type constructor or class" tyThingTyCon_maybe

lookupGhcDataCon :: (GhcLookup n, PPrint n) => Located n -> Ghc DataCon
lookupGhcDataCon = lookupGhcThing "data constructor" tyThingDataCon_maybe

lookupGhcVar :: (GhcLookup n, PPrint n) => Located n -> Ghc Var
lookupGhcVar = lookupGhcThing "variable or data constructor" tyThingId_maybe

--------------------------------------------------------------------------------
-- Optional Lookup Combinator --------------------------------------------------
--------------------------------------------------------------------------------

tryLookup :: (Located n -> Ghc a) -> n -> Ghc (Maybe a)
tryLookup f =
  ghandle catchNotInScope . ghandle catchSourceError . fmap Just . f . dummyLoc
  where
    catchSourceError :: SourceError -> Ghc (Maybe a)
    catchSourceError _ = return Nothing

    catchNotInScope :: Error -> Ghc (Maybe a)
    catchNotInScope ErrNotInScope{} = return Nothing
    catchNotInScope e               = throwGhc e

