{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Bare.Spec (
    makeClasses
  , makeQualifiers
  
  , makeHints
  , makeLVar
  , makeLazy
  , makeHMeas
  , makeTExpr

  , makeTargetVars
  , makeAssertSpec
  , makeAssumeSpec
  , makeDefaultMethods

  , makeIAliases
  , makeInvariants
  ) where

import TyCon
import Var

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Maybe
import Data.Monoid

import qualified Data.List           as L
import qualified Data.HashSet        as S
import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Misc (concatMapM, group, snd3)
import Language.Fixpoint.Names (dropModuleNames, dropSym, isPrefixOfSym, qualifySymbol, takeModuleNames)
import Language.Fixpoint.Types (Qualifier(..), Symbol, symbol)

import Language.Haskell.Liquid.GhcMisc (getSourcePos, showPpr, symbolTyVar)
import Language.Haskell.Liquid.Misc (addFst3, fourth4)
import Language.Haskell.Liquid.RefType (generalize, rVar)
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.Measure as Ms

import Language.Haskell.Liquid.Bare.Check (checkDefAsserts)
import Language.Haskell.Liquid.Bare.Env
import Language.Haskell.Liquid.Bare.Lookup
import Language.Haskell.Liquid.Bare.Misc (joinVar, symbolRTyVar)
import Language.Haskell.Liquid.Bare.Resolve
import Language.Haskell.Liquid.Bare.Type

-- TODO: Find a better name/grouping for this?

makeClasses cfg vs (mod, spec) = inModule mod $ mapM mkClass $ Ms.classes spec
  where
    --FIXME: cleanup this code
    unClass = snd . bkClass . fourth4 . bkUniv
    mkClass (RClass c ss as ms)
            = do let l   = loc c  
                 tc  <- lookupGhcTyCon c
                 ss' <- mapM (mkSpecType l) ss
                 let (dc:_) = tyConDataCons tc
                 let αs  = map symbolRTyVar as
                 let as' = [rVar $ symbolTyVar a | a <- as ]
                 let ms' = [ (s, rFun "" (RApp c (flip RVar mempty <$> as) [] mempty) t) | (s, t) <- ms]
                 vts <- makeSpec cfg vs ms'
                 let sts = [(val s, unClass $ val t) | (s, _)    <- ms
                                                     | (_, _, t) <- vts]
                 let t   = rCls tc as'
                 let dcp = DataConP l αs [] [] ss' (reverse sts) t
                 return ((dc,dcp),vts)

makeQualifiers (mod,spec) = inModule mod mkQuals
  where
    mkQuals = mapM (\q -> resolve (q_pos q) q) $ Ms.qualifiers spec

makeHints vs (_, spec) = varSymbols id "Hint" vs $ Ms.decr spec
makeLVar  vs (_, spec) = fmap fst <$> (varSymbols id "LazyVar" vs $ [(v, ()) | v <- Ms.lvars spec])
makeLazy  vs (_, spec) = fmap fst <$> (varSymbols id "Lazy" vs $ [(v, ()) | v <- S.toList $ Ms.lazy spec])
makeHMeas vs (_, spec) = fmap fst <$> (varSymbols id "HMeas" vs $ [(v, loc v) | v <- S.toList $ Ms.hmeas spec])
makeTExpr vs (_, spec) = varSymbols id "TermExpr" vs $ Ms.termexprs spec

-- TODO: Document/clarify what this does
-- TODO: Using a tuple seems unnecessary, as `ns` is never touched
-- TODO: `_` isn't used; remove?
varSymbols :: ([Var] -> [Var]) -> Symbol ->  [Var] -> [(LocSymbol, a)] -> BareM [(Var, a)]
varSymbols f _ vs  = concatMapM go
  where lvs        = M.map L.sort $ group [(sym v, locVar v) | v <- vs]
        sym        = dropModuleNames . symbol . showPpr
        locVar v   = (getSourcePos v, v)
        go (s, ns) = case M.lookup (val s) lvs of 
                     Just lvs -> return ((, ns) <$> varsAfter f s lvs)
                     Nothing  -> ((:[]).(,ns)) <$> lookupGhcVar s

varsAfter f s lvs 
  | eqList (fst <$> lvs)    = f (snd <$> lvs)
  | otherwise               = map snd $ takeEqLoc $ dropLeLoc lvs
  where
    takeEqLoc xs@((l, _):_) = L.takeWhile ((l==) . fst) xs
    takeEqLoc []            = []
    dropLeLoc               = L.dropWhile ((loc s >) . fst)
    eqList []               = True
    eqList (x:xs)           = all (==x) xs


------------------------------------------------------------------
-- | API: Bare Refinement Types ----------------------------------
------------------------------------------------------------------

makeTargetVars :: ModName -> [Var] -> [String] -> BareM [Var]
makeTargetVars name vs ss
  = do env   <- gets hscEnv
       ns    <- liftIO $ concatMapM (lookupName env name . dummyLoc . prefix) ss
       return $ filter ((`elem` ns) . varName) vs
    where
       prefix s = qualifySymbol (symbol name) (symbol s)


makeAssertSpec cmod cfg vs lvs (mod,spec)
  | cmod == mod
  = makeLocalSpec cfg cmod vs lvs (Ms.sigs spec ++ Ms.localSigs spec)
  | otherwise
  = inModule mod $ makeSpec cfg vs $ Ms.sigs spec

makeAssumeSpec cmod cfg vs lvs (mod,spec)
  | cmod == mod
  = makeLocalSpec cfg cmod vs lvs $ Ms.asmSigs spec
  | otherwise
  = inModule mod $ makeSpec cfg vs $ Ms.asmSigs spec

makeDefaultMethods :: [Var] -> [(ModName,Var,Located SpecType)]
                   -> [(ModName,Var,Located SpecType)]
makeDefaultMethods defVs sigs
  = [ (m,dmv,t)
    | dmv <- defVs
    , let dm = symbol $ showPpr dmv
    , "$dm" `isPrefixOfSym` (dropModuleNames dm)
    , let mod = takeModuleNames dm
    , let method = qualifySymbol mod $ dropSym 3 (dropModuleNames dm)
    , let mb = L.find ((method `isPrefixOfSym`) . symbol . snd3) sigs
    , isJust mb
    , let Just (m,_,t) = mb
    ]

makeLocalSpec :: Config -> ModName -> [Var] -> [Var] -> [(LocSymbol, BareType)]
                    -> BareM [(ModName, Var, Located SpecType)]
makeLocalSpec cfg mod vs lvs xbs
  = do env   <- get
       vbs1  <- fmap expand3 <$> varSymbols fchoose "Var" lvs (dupSnd <$> xbs1)
       unless (noCheckUnknown cfg)   $ checkDefAsserts env vbs1 xbs1
       vts1  <- map (addFst3 mod) <$> mapM mkVarSpec vbs1
       vts2  <- makeSpec cfg vs xbs2
       return $ vts1 ++ vts2
  where
    (xbs1, xbs2)        = L.partition (modElem mod . fst) xbs
    dupSnd (x, y)       = (dropMod x, (x, y))
    expand3 (x, (y, w)) = (x, y, w)
    dropMod             = fmap (dropModuleNames . symbol)
    fchoose ls          = maybe ls (:[]) $ L.find (`elem` vs) ls
    modElem n x         = (takeModuleNames $ val x) == (symbol n)

makeSpec :: Config -> [Var] -> [(LocSymbol, BareType)]
                -> BareM [(ModName, Var, Located SpecType)]
makeSpec cfg vs xbs
  = do vbs <- map (joinVar vs) <$> lookupIds xbs
       env@(BE { modName = mod}) <- get
       unless (noCheckUnknown cfg) $ checkDefAsserts env vbs xbs
       map (addFst3 mod) <$> mapM mkVarSpec vbs


lookupIds = mapM lookup
  where
    lookup (s, t) = (,s,t) <$> lookupGhcVar s

mkVarSpec :: (Var, LocSymbol, BareType) -> BareM (Var, Located SpecType)
mkVarSpec (v, Loc l _, b) = tx <$> mkSpecType l b
  where
    tx = (v,) . Loc l . generalize


makeIAliases (mod, spec)
  = inModule mod $ makeIAliases' $ Ms.ialiases spec

makeIAliases' :: [(Located BareType, Located BareType)] -> BareM [(Located SpecType, Located SpecType)]
makeIAliases' ts = mapM mkIA ts
  where 
    mkIA (t1, t2)      = liftM2 (,) (mkI t1) (mkI t2)
    mkI (Loc l t)      = (Loc l) . generalize <$> mkSpecType l t

makeInvariants (mod,spec)
  = inModule mod $ makeInvariants' $ Ms.invariants spec

makeInvariants' :: [Located BareType] -> BareM [Located SpecType]
makeInvariants' ts = mapM mkI ts
  where 
    mkI (Loc l t)  = (Loc l) . generalize <$> mkSpecType l t

