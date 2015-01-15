{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Bare.GhcSpec (
    GhcSpec(..)
  , makeGhcSpec
  ) where

import CoreSyn hiding (Expr)
import HscTypes
import Id
import NameSet
import TyCon
import Var

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Maybe
import Data.Monoid

import qualified Control.Exception   as Ex 
import qualified Data.List           as L
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

import Language.Fixpoint.Misc (mapSnd, thd3)
import Language.Fixpoint.Names (takeWhileSym)
import Language.Fixpoint.Types (Expr, SEnv, SortedReft, Symbol, TCEmb, fromListSEnv, insertSEnv, mkSubst, subst, substa, symbol)

import Language.Haskell.Liquid.GhcMisc (getSourcePos, sourcePosSrcSpan)
import Language.Haskell.Liquid.PredType (makeTyConInfo)
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.WiredIn

import qualified Language.Haskell.Liquid.Measure as Ms

import Language.Haskell.Liquid.Bare.Check
import Language.Haskell.Liquid.Bare.ConType
import Language.Haskell.Liquid.Bare.Dictionaries
import Language.Haskell.Liquid.Bare.Env
import Language.Haskell.Liquid.Bare.Existential
import Language.Haskell.Liquid.Bare.Measure
import Language.Haskell.Liquid.Bare.Misc (makeSymbols, mkVarExpr)
import Language.Haskell.Liquid.Bare.Plugged
import Language.Haskell.Liquid.Bare.RTEnv
import Language.Haskell.Liquid.Bare.Spec
import Language.Haskell.Liquid.Bare.Sort

------------------------------------------------------------------
---------- Top Level Output --------------------------------------
------------------------------------------------------------------

makeGhcSpec :: Config -> ModName -> [CoreBind] -> [Var] -> [Var] -> NameSet -> HscEnv -> Either Error LogicMap
            -> [(ModName,Ms.BareSpec)]
            -> IO GhcSpec
makeGhcSpec cfg name cbs vars defVars exports env lmap specs
  
  = do sp <- throwLeft =<< execBare act initEnv
       let env = ghcSpecEnv sp
       throwLeft $ checkGhcSpec specs env $ postProcess cbs env sp
  where
    act       = makeGhcSpec' cfg cbs vars defVars exports specs
    throwLeft = either Ex.throw return
    initEnv   = BE name mempty mempty mempty env lmap'
    lmap'     = case lmap of {Left e -> Ex.throw e; Right x -> x}
    
postProcess :: [CoreBind] -> SEnv SortedReft -> GhcSpec -> GhcSpec
postProcess cbs specEnv sp@(SP {..}) = sp { tySigs = tySigs', texprs = ts, asmSigs = asmSigs', dicts = dicts' }
  -- HEREHEREHEREHERE (addTyConInfo stuff) 
  where
    (sigs, ts) = replaceLocalBinds tcEmbeds tyconEnv tySigs texprs specEnv cbs
    tySigs'  = mapSnd (addTyConInfo tcEmbeds tyconEnv <$>) <$> sigs
    asmSigs' = mapSnd (addTyConInfo tcEmbeds tyconEnv <$>) <$> asmSigs
    dicts'   = dmapty (addTyConInfo tcEmbeds tyconEnv) dicts

ghcSpecEnv sp        = fromListSEnv binds
  where 
    emb              = tcEmbeds sp
    binds            =  [(x,        rSort t) | (x, Loc _ t) <- meas sp]
                     ++ [(symbol v, rSort t) | (v, Loc _ t) <- ctors sp]
                     ++ [(x,        vSort v) | (x, v) <- freeSyms sp, isConLikeId v]
    rSort            = rTypeSortedReft emb 
    vSort            = rSort . varRSort 
    varRSort         :: Var -> RSort
    varRSort         = ofType . varType

------------------------------------------------------------------------------------------------
makeGhcSpec' :: Config -> [CoreBind] -> [Var] -> [Var] -> NameSet -> [(ModName, Ms.BareSpec)] -> BareM GhcSpec
------------------------------------------------------------------------------------------------
makeGhcSpec' cfg cbs vars defVars exports specs
  = do name                                    <- gets modName
       makeRTEnv specs
       (tycons, datacons, dcSs, tyi, embs)     <- makeGhcSpecCHOP1 specs
       modify                                   $ \be -> be { tcEnv = tyi }
       (cls, mts)                              <- second mconcat . unzip . mconcat <$> mapM (makeClasses cfg vars) specs
       (invs, ialias, sigs, asms)              <- makeGhcSpecCHOP2 cfg vars defVars specs name mts embs
       (measures, cms', ms', cs', xs')         <- makeGhcSpecCHOP3 cbs specs dcSs datacons cls embs
       syms                                    <- makeSymbols (vars ++ map fst cs') xs' (sigs ++ asms ++ cs') ms' (invs ++ (snd <$> ialias))
       let su  = mkSubst [ (x, mkVarExpr v) | (x, v) <- syms]
       return (emptySpec cfg)
         >>= makeGhcSpec0 cfg defVars exports name
         >>= makeGhcSpec1 vars embs tyi exports name sigs asms cs' ms' cms' su 
         >>= makeGhcSpec2 invs ialias measures su                     
         >>= makeGhcSpec3 datacons tycons embs syms             
         >>= makeGhcSpec4 defVars specs name su 
         >>= makeSpecDictionaries embs vars specs

emptySpec     :: Config -> GhcSpec
emptySpec cfg = SP [] [] [] [] [] [] [] [] [] mempty [] [] [] [] mempty mempty cfg mempty [] mempty dempty


makeGhcSpec0 cfg defVars exports name sp
  = do targetVars <- makeTargetVars name defVars $ binders cfg
       return      $ sp { config = cfg         
                        , exports = exports    
                        , tgtVars = targetVars }

makeGhcSpec1 vars embs tyi exports name sigs asms cs' ms' cms' su sp
  = do tySigs      <- makePluggedSigs name embs tyi exports $ tx sigs
       asmSigs     <- makePluggedAsmSigs embs tyi $ tx asms
       ctors       <- makePluggedAsmSigs embs tyi $ tx cs'
       return $ sp { tySigs     = tySigs
                   , asmSigs    = asmSigs
                   , ctors      = ctors
                   , meas       = tx' $ tx $ ms' ++ varMeasures vars ++ cms' }
    where
      tx   = fmap . mapSnd . subst $ su
      tx'  = fmap (mapSnd $ fmap uRType)

makeGhcSpec2 invs ialias measures su sp
  = return $ sp { invariants = subst su invs 
                , ialiases   = subst su ialias 
                , measures   = subst su <$> M.elems $ Ms.measMap measures }

makeGhcSpec3 datacons tycons embs syms sp
  = do tcEnv   <- gets tcEnv
       return  $ sp { tyconEnv   = tcEnv
                    , dconsP     = datacons
                    , tconsP     = tycons
                    , tcEmbeds   = embs 
                    , freeSyms   = [(symbol v, v) | (_, v) <- syms] }

makeGhcSpec4 defVars specs name su sp
  = do decr'   <- mconcat <$> mapM (makeHints defVars) specs
       texprs' <- mconcat <$> mapM (makeTExpr defVars) specs
       lazies  <- mkThing makeLazy
       lvars'  <- mkThing makeLVar
       hmeas   <- mkThing makeHMeas
       quals   <- mconcat <$> mapM makeQualifiers specs
       return   $ sp { qualifiers = subst su quals
                     , decr       = decr'
                     , texprs     = texprs'
                     , lvars      = lvars'
                     , lazy       = lazies 
                     , tySigs     = strengthenHaskellMeasures hmeas ++ tySigs sp}        
    where
       mkThing mk = S.fromList . mconcat <$> sequence [ mk defVars (m, s) | (m, s) <- specs, m == name ]


makeGhcSpecCHOP1 specs
  = do (tcs, dcs)      <- mconcat <$> mapM makeConTypes specs
       let tycons       = tcs        ++ wiredTyCons 
       let tyi          = makeTyConInfo tycons
       embs            <- mconcat <$> mapM makeTyConEmbeds specs
       datacons        <- makePluggedDataCons embs tyi (concat dcs ++ wiredDataCons)
       let dcSelectors  = concat $ map makeMeasureSelectors datacons
       return           $ (tycons, second val <$> datacons, dcSelectors, tyi, embs) 

makeGhcSpecCHOP2 cfg vars defVars specs name mts embs
  = do sigs'   <- mconcat <$> mapM (makeAssertSpec name cfg vars defVars) specs
       asms'   <- mconcat <$> mapM (makeAssumeSpec name cfg vars defVars) specs
       invs    <- mconcat <$> mapM makeInvariants specs
       ialias  <- mconcat <$> mapM makeIAliases   specs
       let dms  = makeDefaultMethods vars mts
       tyi     <- gets tcEnv
       let sigs = [ (x, txRefSort tyi embs . txExpToBind <$> t) | (_, x, t) <- sigs' ++ mts ++ dms ]
       let asms = [ (x, txRefSort tyi embs . txExpToBind <$> t) | (_, x, t) <- asms' ]
       return     (invs, ialias, sigs, asms)

makeGhcSpecCHOP3 cbs specs dcSelectors datacons cls embs
  = do measures'       <- mconcat <$> mapM makeMeasureSpec specs
       tyi             <- gets tcEnv
       name            <- gets modName 
       hmeans          <- mapM (makeHaskellMeasures cbs name) specs
       let measures     = mconcat (measures':Ms.mkMSpec' dcSelectors:hmeans)
       let (cs, ms)     = makeMeasureSpec' measures
       let cms          = makeClassMeasureSpec measures
       let cms'         = [ (x, Loc l $ cSort t) | (Loc l x, t) <- cms ]
       let ms'          = [ (x, Loc l t) | (Loc l x, t) <- ms, isNothing $ lookup x cms' ]
       let cs'          = [ (v, Loc (getSourcePos v) (txRefSort tyi embs t)) | (v, t) <- meetDataConSpec cs (datacons ++ cls)]
       let xs'          = val . fst <$> ms
       return (measures, cms', ms', cs', xs')


-- TODO: Section Heading
-- TODO: Migrate traversals, maps, etc. to separate module (?)

-- RJ: This is not nice. More than 3 elements should be a record.
    
type ReplaceM = ReaderT ( M.HashMap Symbol Symbol
                        , SEnv SortedReft
                        , TCEmb TyCon
                        , M.HashMap TyCon RTyCon
                        ) (State ( M.HashMap Var (Located SpecType)
                                 , M.HashMap Var [Expr]
                                 ))

replaceLocalBinds :: TCEmb TyCon
                  -> M.HashMap TyCon RTyCon
                  -> [(Var, Located SpecType)]
                  -> [(Var, [Expr])]
                  -> SEnv SortedReft
                  -> CoreProgram
                  -> ([(Var, Located SpecType)], [(Var, [Expr])])
replaceLocalBinds emb tyi sigs texprs senv cbs
  = (M.toList s, M.toList t)
  where
    (s,t) = execState (runReaderT (mapM_ (`traverseBinds` return ()) cbs)
                                  (M.empty, senv, emb, tyi))
                      (M.fromList sigs, M.fromList texprs)

traverseExprs (Let b e)
  = traverseBinds b (traverseExprs e)
traverseExprs (Lam _ e)
  = traverseExprs e
traverseExprs (App x y)
  = traverseExprs x >> traverseExprs y
traverseExprs (Case e _ _ as)
  = traverseExprs e >> mapM_ (traverseExprs . thd3) as
traverseExprs (Cast e _)
  = traverseExprs e
traverseExprs (Tick _ e)
  = traverseExprs e
traverseExprs _
  = return ()

traverseBinds b k
  = do (env', fenv', emb, tyi) <- ask
       let env  = L.foldl' (\m v -> M.insert (takeWhileSym (/='#') $ symbol v) (symbol v) m) env' vs
           fenv = L.foldl' (\m v -> insertSEnv (symbol v) (rTypeSortedReft emb (ofType $ varType v :: RSort)) m) fenv' vs
       withReaderT (const (env,fenv,emb,tyi)) $ do
         mapM_ replaceLocalBindsOne vs
         mapM_ traverseExprs es
         k
  where
    vs = bindersOf b
    es = rhssOfBind b

replaceLocalBindsOne :: Var -> ReplaceM ()
replaceLocalBindsOne v
  = do mt <- gets (M.lookup v . fst)
       case mt of
         Nothing -> return ()
         Just (Loc l (toRTypeRep -> t@(RTypeRep {..}))) -> do
           (env',fenv,emb,tyi) <- ask
           let f m k = M.lookupDefault k k m
           let (env,args) = L.mapAccumL (\e (v,t) -> (M.insert v v e, substa (f e) t))
                             env' (zip ty_binds ty_args)
           let res  = substa (f env) ty_res
           let t'   = fromRTypeRep $ t { ty_args = args, ty_res = res }
           let msg  = ErrTySpec (sourcePosSrcSpan l) (pprint v) t'
           case checkTy msg emb tyi fenv t' of
             Just err -> Ex.throw err
             Nothing -> modify (first $ M.insert v (Loc l t'))
           mes <- gets (M.lookup v . snd)
           case mes of
             Nothing -> return ()
             Just es -> do
               let es'  = substa (f env) es
               case checkTerminationExpr emb fenv (v, Loc l t', es') of
                 Just err -> Ex.throw err
                 Nothing  -> modify (second $ M.insert v es')
