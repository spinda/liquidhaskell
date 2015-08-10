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
  , extractMeasures

    -- * Extraction Functions in ReifyM
  , ReifyM
  , runReifyM
  , extractTySigs
  , extractCtors
  , extractTySyns
  ) where

import GHC hiding (Located)

import Annotations
import Bag
import CoreSyn
import HscTypes
import Id
import MonadUtils
import Name
import NameEnv
import Panic
import Serialized
import TcRnTypes
import Type (eqType)
import Var
import VarEnv

import Control.Arrow (first, second)
import Control.Monad.Reader

import Data.Data (Data)
import Data.Either
import Data.List
import Data.Maybe
import Data.Typeable (Typeable)

import qualified Data.HashMap.Strict as M

import Text.Parsec.Pos

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Errors ()
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Measure
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.TH.Types as LT

import Language.Haskell.Liquid.Spec.CoreToLogic
import Language.Haskell.Liquid.Spec.Reify

--------------------------------------------------------------------------------
-- ExtractM Monad --------------------------------------------------------------
--------------------------------------------------------------------------------

-- TOOD: Merge this with ReifyM into one shared SpecM
newtype ExtractM a = ExtractM { unExtractM :: ReaderT ExtractEnv Ghc a }
                     deriving (Functor, Applicative, Monad)

data ExtractEnv = ER { er_hscSource   :: !HscSource
                     , er_scope       :: !GhcSpec
                     , er_annotations :: ![Annotation]
                     , er_coreEnv     :: !(VarEnv CoreExpr)
                     }


runExtractM :: ExtractM a -> HscSource -> GhcSpec -> [Annotation] -> [CoreBind] -> Ghc a
runExtractM act src scope anns cbs = runReaderT (unExtractM act) initEnv
  where
    initEnv = ER src scope anns (buildCoreEnv cbs)

buildCoreEnv :: [CoreBind] -> VarEnv CoreExpr
buildCoreEnv = mkVarEnv . concatMap go
  where
    go (NonRec v def) = [(v, def)]
    go (Rec xes)      = xes


liftGhc :: Ghc a -> ExtractM a
liftGhc = ExtractM . lift

getHscSource :: ExtractM HscSource
getHscSource = ExtractM $ asks er_hscSource

getScope :: ExtractM GhcSpec
getScope = ExtractM $ asks er_scope

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

-- General Annotations ---------------------------------------------------------

extractExprParams :: ExtractM (NameEnv [Symbol])
extractExprParams = mkNameEnv <$> (mapM go =<< annotationsOfType)
  where
    go (name, LT.ExprParams params) =
      return (name, symbol <$> LT.loc_value params)

extractTcEmbeds :: ExtractM [(TyCon, Located FTycon)]
extractTcEmbeds = mapM go =<< annotationsOfType
  where
    go (name, LT.EmbedAs ftc) = do
      Just (ATyCon tc) <- liftGhc $ lookupName name
      return (tc, convertFTycon $ convertLocated ftc)

-- Lift to Logic ---------------------------------------------------------------

extractInlines :: ExtractM [(Located Var, TInline)]
extractInlines = liftToLogic "an inline" tinlines coreToFun ofAnn ofLifted ofAlias
  where
    ofAnn (LT.IsInline span alias) =
      (span, alias)
    ofLifted _ _ (xs, e) =
      TI (symbol <$> xs) e
    ofAlias _ = id

extractMeasures :: ExtractM [(Located Var, SpecMeasure)]
extractMeasures = liftToLogic "a measure" meas coreToDef ofAnn ofLifted ofAlias
  where
    ofAnn (LT.IsMeasure span alias) =
      (span, alias)
    ofLifted sym var defs =
      M sym (logicType $ varType var) (dataConTypes defs)
    ofAlias sym m =
      m { name = sym
        , defs = map (second $ subst $ mkSubst [(val $ name m, EVar $ val sym)]) $ defs m
        }


-- TODO: This function sprawls out a bit right now, but it'll get
--       cleaner once we merge ExtractM and ReifyM and are progressively
--       updating shared state
liftToLogic :: (Data b, Typeable b)
            => String
            -> (GhcSpec -> M.HashMap Var c)
            -> (LocSymbol -> Var -> CoreExpr -> LogicM a)
            -> (b -> (LT.Span, Maybe (LT.Located String)))
            -> (LocSymbol -> Var -> a -> c)
            -> (LocSymbol -> c -> c)
            -> ExtractM [(Located Var, c)]
liftToLogic kind ofScope ofBind ofAnn ofLifted ofAlias = do
  (aliases, lifted) <- partitionEithers <$> (mapM go_ann =<< annotationsOfType)
  checkAliasesAllowed aliases

  let liftedMap      = M.fromList $ map (first val) lifted
  totalMap          <- mappend liftedMap . ofScope <$> getScope
  aliases'          <- mapM (go_alias totalMap) aliases

  return (aliases' ++ lifted)
  where
    go_ann (name, ofAnn -> (span, alias)) = do
      Just (AnId var) <- liftGhc $ lookupName name
      let lvar         = convertLocated' span var
      let sym          = varSymbol <$> lvar
      case alias of
        Just target -> do
          tgtVar <- liftGhc $ lookupVar $ convertLocated target
          return $ Left (sym, lvar, tgtVar)
        Nothing -> do
          Just bind <- lookupCoreBind var
          case runToLogic mempty $ ofBind sym var bind of
            Left  res -> return $ Right (lvar, ofLifted sym var res)
            Right err -> pgmError err
    go_alias totalMap (sym, lvar, tgtVar)
      | tyCompat (val lvar) tgtVar =
        case M.lookup tgtVar totalMap of
          Nothing -> pgmError $ "Not " ++ kind ++ ": " ++ showPpr tgtVar
          Just lg -> return (lvar, ofAlias sym lg)
      | otherwise =
        liftGhc $ throwGhc $
          (ErrAliasTypes (locatedSrcSpan lvar) (pprint $ val lvar) (pprint tgtVar)
                         (varType $ val lvar) (varType tgtVar)
                           :: Error)

    tyCompat lhs rhs = varType lhs `eqType` varType rhs

    checkAliasesAllowed [] =
      return ()
    checkAliasesAllowed ((_, lv, _):_) = do
      allowed <- isHsBootOrSig <$> getHscSource
      if allowed
         then return ()
         else liftGhc $ throwGhc $
                (ErrAliasCtxt (locatedSrcSpan lv) (pprint $ val lv)
                  :: Error)

-- Utility Functions -----------------------------------------------------------

lookupVar :: Located String -> Ghc Var
lookupVar str = do
  names  <- parseName $ val str
  things <- mapMaybeM lookupName names
  case mapMaybe tyThingId_maybe things of
    (v:_) | not (isDataConId v) ->
      return v
    _ ->
      pgmError $ "Not in scope: variable `" ++ (val str) ++ "'"


convertFTycon :: Located LT.FTycon -> Located FTycon
convertFTycon lf@(Loc start end _) = go <$> lf
  where
    go LT.FTcInt      = intFTyCon
    go LT.FTcReal     = realFTyCon
    go LT.FTcBool     = boolFTyCon
    go (LT.FTcUser s) = symbolFTycon $ Loc start end $ symbol s

convertLocated :: LT.Located a -> Located a
convertLocated (LT.Located span value) = convertLocated' span value

convertLocated' :: LT.Span -> a -> Located a
convertLocated' span value = Loc start end value
  where (start, end) = convertSpan span

convertSpan :: LT.Span -> (SourcePos, SourcePos)
convertSpan (LT.Span start end) = (convertPos start, convertPos end)

convertPos :: LT.Pos -> SourcePos
convertPos (LT.Pos name line col) = newPos name line col 

--------------------------------------------------------------------------------
-- Extraction Functions in ReifyM ----------------------------------------------
--------------------------------------------------------------------------------

extractTySigs :: TypecheckedModule -> ReifyM (M.HashMap Var SpecType)
extractTySigs mod = M.fromList <$> mapM (\id -> (id, ) <$> reifyRTy (idType id)) ids
  where
    ids        = nub $ filter (\x -> not (isDictId x) && not (isDataConId x)) (topLevel ++ fromSource)
    fromSource = idsFromSource (tm_typechecked_source mod)
    topLevel   = mapMaybe tyThingId_maybe $ typeEnvElts $ tcg_type_env $ fst $ tm_internals_ mod

extractCtors :: TypecheckedModule -> ReifyM (M.HashMap Var SpecType)
extractCtors mod = M.fromList <$> concatMapM (\dc -> mkDataConIdsTy dc <$> reifyRTy (dataConUserType dc)) dcs
  where
    dcs = mapMaybe tyThingDataCon_maybe $ typeEnvElts $ tcg_type_env $ fst $ tm_internals_ mod

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

-- TODO: This may be duplicating information that we already gather in
--       Plugin.Ghc

idsFromSource :: TypecheckedSource -> [Id]
idsFromSource = filter (not . isDataConId) . idsFromBinds

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

