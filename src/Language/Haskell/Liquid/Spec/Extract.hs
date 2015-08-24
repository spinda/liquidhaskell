{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Spec.Extract (
    -- * Extract Specifications from Annotations
    extractGhcSpec
  ) where

import GHC hiding (Located)

import Annotations
import Class
import CoreSyn
import DataCon
import Id
import IdInfo
import Name
import NameEnv
import Serialized
import TyCon
import Type
import Var
import VarEnv
import VarSet

import Control.Arrow
import Control.Monad
import Control.Monad.Trans

import Data.Data (Data)
import Data.List
import Data.Maybe
import Data.Typeable (Typeable)

import qualified Data.HashMap.Strict as M

import qualified Language.Haskell.TH.Syntax as TH

import Text.PrettyPrint.HughesPJ hiding (first)

import Language.Fixpoint.Misc
import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Measure
import Language.Haskell.Liquid.PredType hiding (freeTyVars)
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types hiding (freeTyVars)

import Language.Haskell.Liquid.TH.Types

import Language.Haskell.Liquid.Spec.Check
import Language.Haskell.Liquid.Spec.CoreToLogic
import Language.Haskell.Liquid.Spec.Lookup
import Language.Haskell.Liquid.Spec.Reify

--------------------------------------------------------------------------------
-- Extract Specifications from Annotations -------------------------------------
--------------------------------------------------------------------------------

extractGhcSpec :: Module -> Maybe Module -> [Var] -> [Annotation] -> [CoreBind] -> Ghc GhcSpec
extractGhcSpec mod sig letVs anns cbs = do
  (tySigs', asmSigs', ctors') <- extractVarSpecs (isJust sig) letVs anns
  rtEnv'                      <- extractSynSpecs anns
  tcEmbeds'                   <- extractTcEmbeds mod sig anns
  (tinlines', meas')          <- extractLiftedLogic anns cbs
  return $
    mempty { tySigs   = tySigs'
           , asmSigs  = asmSigs'
           , ctors    = ctors'
           , rtEnv    = rtEnv'
           , tcEmbeds = tcEmbeds'
           , tinlines = tinlines'
           , meas     = meas'
           }


extractVarSpecs :: Bool
                -> [Var]
                -> [Annotation]
                -> Ghc ( M.HashMap Var (Located SpecType)
                       , M.HashMap Var (Located SpecType)
                       , M.HashMap Var (Located SpecType)
                       )
extractVarSpecs isSig letVs anns = do
  sigs <- mapM ofAnn $ annotationsOfType anns
  let (tys, asms, ctors) = foldl' categorize ([],[],[]) sigs
  return (M.fromList tys, M.fromList asms, M.fromList ctors)
  where
    ofAnn (name, LiquidVar full assumed ty span) = do
      var <- lookupVar $ spanLocated span name
      ty' <- traverse reifyRTy ty
      return (var, assumed, completeRTy full var <$> ty')
    categorize (tys, asms, ctors) (var, assumed, ty)
      | isDataConId var  = (tys, asms, (var, ty):ctors)
      | assumed || isSig = (tys, (var, ty):asms, ctors)
      | otherwise        = ((var, ty):tys, asms, ctors)
    lookupVar name = case lookupNameEnv varEnv $ val name of
      Just v  -> return v
      Nothing -> lookupGhcVar name
    varEnv = mkNameEnv $ map (\v -> (getName v, v)) letVs

extractSynSpecs :: [Annotation] -> Ghc RTEnv
extractSynSpecs anns = M.fromList <$> mapM ofAnn (annotationsOfType anns)
  where
    ofAnn (name, LiquidSyn evs rhs span) = do
      tc   <- lookupGhcTyCon $ spanLocated span name
      rhs' <- traverse reifyRTy rhs
      let Just (tvs, _) = synTyConDefn_maybe tc
      let tvs'          = map (stringRTyVar . getOccString) tvs
      return (tc, RTA tvs' (map symbol evs) rhs')

extractTcEmbeds :: Module -> Maybe Module -> [Annotation] -> Ghc (TCEmb TyCon)
extractTcEmbeds mod sig anns = do
  emb <- mapM ofAnn $ annotationsOfType anns
  throwsGhc $ checkDupEmbeds emb ++ checkLocalEmbeds mod sig emb
  return $ M.fromList $ second val <$> emb
  where
    ofAnn (name, EmbedAs ftc span) = do
      tc <- lookupGhcTyCon $ spanLocated span name
      return (tc, ftc)

extractLiftedLogic :: [Annotation]
                   -> [CoreBind]
                   -> Ghc ( M.HashMap Var TInline
                          , M.HashMap LocSymbol SpecMeasure
                          )
extractLiftedLogic anns cbs = do
  (inlines, measures) <- foldM ofAnn ([],[]) $ annotationsOfType anns
  throwsGhc $ checkDupLogic (map (fmap varSymbol . fst) inlines) (map fst measures)
  return ( M.fromList $ map (first val) inlines
         , M.fromList measures
         )
  where
    coreEnv = buildCoreEnv cbs

    ofAnn (inlines, measures) (name, LiftToLogic kind span) = do
      var          <- lookupGhcVar $ spanLocated span name
      let sym       = spanLocated span $ varSymbol var
      let Just bind = lookupVarEnv coreEnv var

      let liftToLogic dsc tx = case runToLogic mempty $ tx sym var bind of
            Left  res -> return res
            Right err -> throwGhc $ mkErr span var dsc err

      case kind of
        InlineKind  -> do
          ti <- mkInline <$> liftToLogic "measure" coreToFun
          return ((spanLocated span var, ti) : inlines, measures)
        BoundKind   -> error "extractLiftedLogic: lifting bounds not yet implemented"
        MeasureKind -> do
          me <- mkMeasure var <$> liftToLogic "inline" coreToDef
          return (inlines, (sym, me) : measures)

    mkInline (xs, e) =
      TI (symbol <$> xs) e
    mkMeasure var defs =
      M (logicType $ varType var) (dataConTypes defs)

    mkErr :: SourceSpan -> Var -> String -> String -> Error
    mkErr (SourceSpan start end) var dsc err =
      ErrLiftToLogic (mkSrcSpan' start end) (pprint var) (text dsc) (text err)

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

annotationsOfType :: (Data a, Typeable a) => [Annotation] -> [(Name, a)]
annotationsOfType = mapMaybe go
  where
    go (Annotation (NamedTarget name) payload)
      | Just val <- fromSerialized deserializeWithData payload =
        Just (name, val)
    go _ = Nothing

buildCoreEnv :: [CoreBind] -> VarEnv CoreExpr
buildCoreEnv = mkVarEnv . concatMap go
  where
    go (NonRec v def) = [(v, def)]
    go (Rec xes)      = xes

completeRTy :: Bool -> Var -> SpecType -> SpecType
completeRTy full var ty
  | full = ty
  | isId var, Just cls <- isClassOpId_maybe var =
    let tc  = classTyCon cls
        tvs = map plainRTyVar (classTyVars cls)
    in  quantifyRTy tvs $
          rFun dummySymbol (rApp tc (flip RVar mempty <$> tvs) [] mempty) $
            quantifyRTy (freeTyVars ty \\ tvs) ty
  | isId var, isRecordSelector var =
    let dc    = recordSelectorDataCon var
        dcTy  = dataConOrigResTy dc
        dcTvs = plainRTyVar <$> varSetElemsKvsFirst (tyVarsOfType dcTy)
        tvs   = nub $ dcTvs ++ freeTyVars ty
    in  quantifyRTy tvs $
          foldr (rFun dummySymbol) ty $
            map ofType $ dataConStupidTheta dc ++ [dcTy]
  | otherwise = quantifyFreeRTy ty

