{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Spec.Extract (
    -- * Extraction Functions in ExtractM
    ExtractM
  , runExtractM
  , extractExprParams
  , extractTcEmbeds
  , extractInlines

    -- * Extraction Functions in ReifyM
  , ReifyM
  , runReifyM
  , extractTySigs
  , extractTySyns
  ) where

import GHC hiding (Located)

import Annotations
import Bag
import CoreSyn
import MonadUtils
import Name
import NameEnv
import Panic
import Serialized
import Var
import VarEnv

import Control.Monad.Reader

import Data.Data (Data)
import Data.Maybe
import Data.Typeable (Typeable)

import qualified Data.HashMap.Strict as M

import Text.Parsec.Pos

import Language.Fixpoint.Types

import Language.Haskell.Liquid.CoreToLogic
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.RType as RT

import Language.Haskell.Liquid.Spec.Reify

--------------------------------------------------------------------------------
-- ExtractM Monad --------------------------------------------------------------
--------------------------------------------------------------------------------

newtype ExtractM a = ExtractM { unExtractM :: ReaderT ExtractEnv Ghc a }
                     deriving (Functor, Applicative, Monad)

data ExtractEnv = ER { er_annotations :: [Annotation]
                     , er_coreEnv     :: VarEnv CoreExpr
                     }


runExtractM :: ExtractM a -> [Annotation] -> [CoreBind] -> Ghc a
runExtractM act anns cbs = runReaderT (unExtractM act) initEnv
  where
    initEnv = ER anns (buildCoreEnv cbs)

buildCoreEnv :: [CoreBind] -> VarEnv CoreExpr
buildCoreEnv = mkVarEnv . concatMap go
  where
    go (NonRec v def) = [(v, def)]
    go (Rec xer)      = xer


liftGhc :: Ghc a -> ExtractM a
liftGhc = ExtractM . lift

annotationsOfType :: (Data a, Typeable a) => ExtractM [(Name, a)]
annotationsOfType = ExtractM $ asks (mapMaybe go . er_annotations)
  where
    go (Annotation (NamedTarget name) payload)
      | Just val <- fromSerialized deserializeWithData payload =
        Just (name, val)
    go _ = Nothing

lookupCoreBind :: Var -> ExtractM (Maybe CoreExpr)
lookupCoreBind var = ExtractM $ asks (flip lookupVarEnv var . er_coreEnv)

--------------------------------------------------------------------------------
-- Extraction Functions in ExtractM --------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Error out on duplicate annotations (using provided location info)

extractExprParams :: ExtractM (NameEnv [Symbol])
extractExprParams = mkNameEnv <$> (mapM go =<< annotationsOfType)
  where
    go (name, RT.ExprParams params) =
      return (name, symbol <$> RT.loc_value params)

extractTcEmbeds :: ExtractM (TCEmb TyCon)
extractTcEmbeds = M.fromList <$> (mapM go =<< annotationsOfType)
  where
    go (name, RT.EmbedAs ftc) = do
      Just (ATyCon tc) <- liftGhc $ lookupName name
      return (tc, convertFTycon $ convertLocated ftc)

extractInlines :: ExtractM (VarEnv TInline)
extractInlines = mkVarEnv <$> (mapM go =<< annotationsOfType)
  where
    go (name, RT.IsInline span) = do
      Just (AnId var) <- liftGhc $ lookupName name
      def             <- fromJust <$> lookupCoreBind var
      let sym          = convertLocated' span $ symbol $ getName var
      case runToLogic mempty $ coreToFun sym var def of
        Left (xs, e) -> return (var, TI (symbol <$> xs) e)
        Right err    -> liftGhc $ panic err


convertFTycon :: Located RT.FTycon -> FTycon
convertFTycon (Loc start end value) = go value
  where
    go RT.FTcInt      = intFTyCon
    go RT.FTcReal     = realFTyCon
    go RT.FTcBool     = boolFTyCon
    go (RT.FTcUser s) = symbolFTycon $ Loc start end $ symbol s

convertLocated :: RT.Located a -> Located a
convertLocated (RT.Located span value) = convertLocated' span value

convertLocated' :: RT.Span -> a -> Located a
convertLocated' span value = Loc start end value
  where (start, end) = convertSpan span

convertSpan :: RT.Span -> (SourcePos, SourcePos)
convertSpan (RT.Span start end) = (convertPos start, convertPos end)

convertPos :: RT.Pos -> SourcePos
convertPos (RT.Pos name line col) = newPos name line col 

--------------------------------------------------------------------------------
-- Extraction Functions in ReifyM ----------------------------------------------
--------------------------------------------------------------------------------

extractTySigs :: TypecheckedModule -> ReifyM [(Var, SpecType)]
extractTySigs mod = do
  liftIO $ putStrLn $ showPpr $ tm_typechecked_source mod
  liftIO $ putStrLn $ showPpr ids
  mapM (\id -> (id, ) <$> reifyRTy (idType id)) ids
  where
    ids = idsFromSource $ tm_typechecked_source mod

extractTySyns :: TypecheckedModule -> ReifyM RTEnv
extractTySyns mod = M.fromList <$> mapM go tysyns
  where
    things = modInfoTyThings $ tm_checked_module_info mod
    tycons = mapMaybe (\case { ATyCon tc -> Just tc; _ -> Nothing }) things
    tysyns = mapMaybe (\tc -> (tc, ) <$> synTyConDefn_maybe tc) tycons
    go (tc, (tvs, rhs)) = do
      rhs' <- reifyRTy rhs
      evs  <- lookupExprParams tc
      return $ (tc, ) $
        RTA { rtTArgs = map rTyVar tvs
            , rtEArgs = evs
            , rtBody  = rhs'
            }

--------------------------------------------------------------------------------
-- Collect Ids from Haskell AST ------------------------------------------------
--------------------------------------------------------------------------------

idsFromSource :: TypecheckedSource -> [Id]
idsFromSource = idsFromBinds

idsFromBinds :: LHsBinds Id -> [Id]
idsFromBinds = concatMap idsFromBind . bagToList

-- TODO: Re-check that this always extracts the right Ids
idsFromBind :: LHsBindLR Id Id -> [Id]
idsFromBind (unLoc -> bind@(AbsBinds {})) =
  map abe_poly (abs_exports bind) ++ idsFromBinds (abs_binds bind)
idsFromBind (unLoc -> bind@(FunBind {})) =
  idsFromMatches $ fun_matches bind
idsFromBind _ = []

idsFromMatches :: MatchGroup Id (LHsExpr Id) -> [Id]
idsFromMatches = concatMap idsFromMatch . mg_alts

idsFromMatch :: LMatch Id (LHsExpr Id) -> [Id]
idsFromMatch = idsFromGRHSs . m_grhss . unLoc

idsFromGRHSs :: GRHSs Id (LHsExpr Id) -> [Id]
idsFromGRHSs = idsFromLocalBinds . grhssLocalBinds

idsFromLocalBinds :: HsLocalBinds Id -> [Id]
idsFromLocalBinds (HsValBinds binds) = idsFromValBinds binds
idsFromLocalBinds _                  = []

-- TODO: How does recursion affect this?
idsFromValBinds :: HsValBinds Id -> [Id]
idsFromValBinds (ValBindsIn  binds _) = idsFromBinds binds
idsFromValBinds (ValBindsOut binds _) = concatMap (idsFromBinds . snd) binds

