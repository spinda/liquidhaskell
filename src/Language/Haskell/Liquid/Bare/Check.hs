{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Bare.Check (
    checkGhcSpec

  , checkDefAsserts
  , checkTerminationExpr
  , checkTy
  ) where

import Debug.Trace

import DataCon
import Name (getSrcSpan)
import TyCon
import Var

import Control.Applicative ((<$>), (<|>))
import Control.Monad.Writer
import Data.Maybe
import Text.PrettyPrint.HughesPJ
import Text.Printf

import qualified Data.List           as L
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

import Language.Fixpoint.Misc (applyNonNull, group, mapSnd, snd3)
import Language.Fixpoint.Names (isPrefixOfSym, stripParensSym)
import Language.Fixpoint.Sort (checkSorted, checkSortedReftFull, checkSortFull)
import Language.Fixpoint.Types hiding (R)

import Language.Haskell.Liquid.GhcMisc (showPpr, sourcePosSrcSpan)
import Language.Haskell.Liquid.Misc (dropThd3, firstDuplicate)
import Language.Haskell.Liquid.PredType (pvarRType, wiredSortedSyms)
import Language.Haskell.Liquid.PrettyPrint (pprintSymbol)
import Language.Haskell.Liquid.RefType (classBinds, ofType, rTypeSort, rTypeSortedReft, subsTyVars_meet)
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.Measure as Ms

import Language.Haskell.Liquid.Bare.ConType (dataConSpec)
import Language.Haskell.Liquid.Bare.Env
import Language.Haskell.Liquid.Bare.Sort (txRefSort)

----------------------------------------------------------------------------------------------
----- Checking GhcSpec -----------------------------------------------------------------------
----------------------------------------------------------------------------------------------

checkGhcSpec :: [(ModName, Ms.BareSpec)]
             -> SEnv SortedReft
             -> GhcSpec
             -> Either [Error] GhcSpec

checkGhcSpec specs env sp =  applyNonNull (Right sp) Left errors
  where 
    errors           =  mapMaybe (checkBind "constructor" emb tcEnv env) (dcons      sp)
                     ++ mapMaybe (checkBind "measure"     emb tcEnv env) (meas       sp)
                     ++ mapMaybe (checkInv  emb tcEnv env)               (invariants sp)
                     ++ (checkIAl  emb tcEnv env) (ialiases   sp)
                     ++ checkMeasures emb env ms
                     ++ mapMaybe checkMismatch                     sigs
                     ++ checkDuplicate                             (tySigs sp)
                     ++ checkDuplicate                             (asmSigs sp)
                     ++ checkDupIntersect                          (tySigs sp) (asmSigs sp)
                     ++ checkRTAliases "Type Alias" env            tAliases
                     ++ checkRTAliases "Pred Alias" env            pAliases 
                     ++ checkDuplicateFieldNames                   (dconsP sp)


    tAliases         =  concat [Ms.aliases sp  | (_, sp) <- specs]
    pAliases         =  concat [Ms.paliases sp | (_, sp) <- specs]
    dcons spec       =  [(v, Loc l t) | (v,t)   <- dataConSpec (dconsP spec) 
                                      | (_,dcp) <- dconsP spec
                                      , let l = dc_loc dcp
                                      ]
    emb              =  tcEmbeds sp
    tcEnv            =  tyconEnv sp
    ms               =  measures sp
    sigs             =  tySigs sp ++ asmSigs sp

checkDuplicateFieldNames :: [(DataCon, DataConP)]  -> [Error]
checkDuplicateFieldNames = catMaybes . map go
  where
    go (d, dts)        = checkNoDups (dc_loc dts) d (fst <$> tyArgs dts)
    checkNoDups l d xs = mkErr l d <$> firstDuplicate xs 

    mkErr l d x = ErrBadData (sourcePosSrcSpan l) 
                             (pprint d) 
                             (text "Multiple declarations of record selector" <+> pprintSymbol x)

checkInv :: TCEmb TyCon -> TCEnv -> SEnv SortedReft -> Located SpecType -> Maybe Error
checkInv emb tcEnv env t   = checkTy err emb tcEnv env (val t) 
  where 
    err              = ErrInvt (sourcePosSrcSpan $ loc t) (val t) 

checkIAl :: TCEmb TyCon -> TCEnv -> SEnv SortedReft -> [(Located SpecType, Located SpecType)] -> [Error]
checkIAl emb tcEnv env ials = catMaybes $ concatMap (checkIAlOne emb tcEnv env) ials

checkIAlOne emb tcEnv env (t1, t2) = checkEq : (tcheck <$> [t1, t2])
  where 
    tcheck t = checkTy (err t) emb tcEnv env (val t)
    err    t = ErrIAl (sourcePosSrcSpan $ loc t) (val t) 
    t1'      :: RSort 
    t1'      = toRSort $ val t1
    t2'      :: RSort 
    t2'      = toRSort $ val t2
    checkEq  = if (t1' == t2') then Nothing else Just errmis
    errmis   = ErrIAlMis (sourcePosSrcSpan $ loc t1) (val t1) (val t2) emsg
    emsg     = pprint t1 <+> text "does not match with" <+> pprint t2 


-- FIXME: Should _ be removed if it isn't used?
checkRTAliases msg _ as = err1s
  where 
    err1s                  = checkDuplicateRTAlias msg as

checkBind :: (PPrint v) => String -> TCEmb TyCon -> TCEnv -> SEnv SortedReft -> (v, Located SpecType) -> Maybe Error 
checkBind s emb tcEnv env (v, Loc l t) = checkTy msg emb tcEnv env' t
  where 
    msg                      = ErrTySpec (sourcePosSrcSpan l) (text s <+> pprint v) t 
    env'                     = foldl (\e (x, s) -> insertSEnv x (RR s mempty) e) env wiredSortedSyms

checkTerminationExpr :: (Eq v, PPrint v) => TCEmb TyCon -> SEnv SortedReft -> (v, Located SpecType, [Expr])-> Maybe Error 
checkTerminationExpr emb env (v, Loc l t, es) = (mkErr <$> go es) <|> (mkErr' <$> go' es)
  where 
    mkErr   = uncurry (ErrTermSpec (sourcePosSrcSpan l) (text "termination expression" <+> pprint v))
    mkErr'  = uncurry (ErrTermSpec (sourcePosSrcSpan l) (text "termination expression is not numeric"))
    go      = foldl (\err e -> err <|> fmap (e,) (checkSorted env' e)) Nothing
    go'     = foldl (\err e -> err <|> fmap (e,) (checkSorted env' (cmpZero e))) Nothing
    env'    = foldl (\e (x, s) -> insertSEnv x s e) env'' wiredSortedSyms
    env''   = mapSEnv sr_sort $ foldl (\e (x,s) -> insertSEnv x s e) env xss
    xss     = mapSnd rSort <$> (uncurry zip $ dropThd3 $ bkArrowDeep t)
    rSort   = rTypeSortedReft emb
    cmpZero = PAtom Le zero 

checkTy :: (Doc -> Error) -> TCEmb TyCon -> TCEnv -> SEnv SortedReft -> SpecType -> Maybe Error
checkTy mkE emb tcEnv env t = mkE <$> checkRType emb env (txRefSort tcEnv emb t)

checkDupIntersect     :: [(Var, Located SpecType)] -> [(Var, Located SpecType)] -> [Error]
checkDupIntersect xts mxts = concatMap mkWrn dups
  where 
    mkWrn (x, t)     = pprWrn x (sourcePosSrcSpan $ loc t)
    dups             = L.intersectBy (\x y -> (fst x == fst y)) mxts xts
    pprWrn v l       = trace ("WARNING: Assume Overwrites Specifications for "++ show v ++ " : " ++ showPpr l) []

checkDuplicate        :: [(Var, Located SpecType)] -> [Error]
checkDuplicate xts = mkErr <$> dups
  where
    mkErr (x, ts) = ErrDupSpecs (getSrcSpan x) (pprint x) (sourcePosSrcSpan . loc <$> ts)
    dups          = [z | z@(_, _:_:_) <- M.toList $ group xts ]

checkDuplicateRTAlias :: String -> [RTAlias s a] -> [Error]
checkDuplicateRTAlias s tas = mkErr <$> dups
  where
    mkErr xs@(x:_)          = ErrDupAlias (sourcePosSrcSpan $ rtPos x) 
                                          (text s) 
                                          (pprint $ rtName x) 
                                          (sourcePosSrcSpan . rtPos <$> xs)
    mkErr []                = error "mkError: called on empty list"
    dups                    = [z | z@(_:_:_) <- L.groupBy (\x y -> rtName x == rtName y) tas]



checkMismatch        :: (Var, Located SpecType) -> Maybe Error
checkMismatch (x, t) = if ok then Nothing else Just err
  where 
    ok               = tyCompat x (val t)
    err              = errTypeMismatch x t

tyCompat x t         = lhs == rhs
  where 
    lhs :: RSort     = toRSort t
    rhs :: RSort     = ofType $ varType x

errTypeMismatch     :: Var -> Located SpecType -> Error
errTypeMismatch x t = ErrMismatch (sourcePosSrcSpan $ loc t) (pprint x) (varType x) (val t)

------------------------------------------------------------------------------------------------
-- | @checkRType@ determines if a type is malformed in a given environment ---------------------
------------------------------------------------------------------------------------------------
checkRType :: (PPrint r, Reftable r) => TCEmb TyCon -> SEnv SortedReft -> RRType r -> Maybe Doc 
------------------------------------------------------------------------------------------------

checkRType emb env t         = efoldReft cb (rTypeSortedReft emb) f insertPEnv env Nothing t 
  where 
    cb c ts                  = classBinds (rRCls c ts)
    f env me r err           = err <|> checkReft env emb me r
    insertPEnv p γ           = insertsSEnv γ (mapSnd (rTypeSortedReft emb) <$> pbinds p) 
    pbinds p                 = (pname p, pvarRType p :: RSort) 
                              : [(x, t) | (t, x, _) <- pargs p]


checkReft                    :: (PPrint r, Reftable r) => SEnv SortedReft -> TCEmb TyCon -> Maybe (RRType r) -> r -> Maybe Doc 
checkReft _   _   Nothing _  = Nothing -- TODO:RPropP/Ref case, not sure how to check these yet.  
checkReft env emb (Just t) _ = (dr $+$) <$> checkSortedReftFull env' r 
  where 
    r                        = rTypeSortedReft emb t
    dr                       = text "Sort Error in Refinement:" <+> pprint r 
    env'                     = foldl (\e (x, s) -> insertSEnv x (RR s mempty) e) env wiredSortedSyms

-- DONT DELETE the below till we've added pred-checking as well
-- checkReft env emb (Just t) _ = checkSortedReft env xs (rTypeSortedReft emb t) 
--    where xs                  = fromMaybe [] $ params <$> stripRTypeBase t 

-- checkSig env (x, t) 
--   = case filter (not . (`S.member` env)) (freeSymbols t) of
--       [] -> TrueNGUAGE ScopedTypeVariables #-}
--       ys -> errorstar (msg ys) 
--     where 
--       msg ys = printf "Unkown free symbols: %s in specification for %s \n%s\n" (showpp ys) (showpp x) (showpp t)

---------------------------------------------------------------------------------------------------
-- | @checkMeasures@ determines if a measure definition is wellformed -----------------------------
---------------------------------------------------------------------------------------------------
checkMeasures :: M.HashMap TyCon FTycon -> SEnv SortedReft -> [Measure SpecType DataCon] -> [Error]
---------------------------------------------------------------------------------------------------
checkMeasures emb env = concatMap (checkMeasure emb env)

checkMeasure :: M.HashMap TyCon FTycon -> SEnv SortedReft -> Measure SpecType DataCon -> [Error]
checkMeasure emb γ (M name@(Loc src n) sort body)
  = [txerror e | Just e <- checkMBody γ emb name sort <$> body]
  where 
    txerror = ErrMeas (sourcePosSrcSpan src) n

checkMBody γ emb _ sort (Def _ c bs body) = checkMBody' emb sort γ' body
  where 
    γ'   = L.foldl' (\γ (x, t) -> insertSEnv x t γ) γ xts
    xts  = zip bs $ rTypeSortedReft emb . subsTyVars_meet su <$> ty_args trep
    trep = toRTypeRep ct
    su   = checkMBodyUnify (ty_res trep) (head $ snd3 $ bkArrowDeep sort)
    ct   = ofType $ dataConUserType c :: SpecType

checkMBodyUnify                 = go
  where
    go (RVar tv _) t            = [(tv, toRSort t, t)]
    go t@(RApp {}) t'@(RApp {}) = concat $ zipWith go (rt_args t) (rt_args t')
    go _ _                      = []

checkMBody' emb sort γ body = case body of
    E e   -> checkSortFull γ (rTypeSort emb sort') e
    P p   -> checkSortFull γ psort  p
    R s p -> checkSortFull (insertSEnv s sty γ) psort p
  where
    psort = FApp propFTyCon []
    sty   = rTypeSortedReft emb sort' 
    sort' = fromRTypeRep $ trep' { ty_vars  = [], ty_preds = [], ty_labels = []
                                 , ty_binds = tail $ ty_binds trep'
                                 , ty_args  = tail $ ty_args trep'             }
    trep' = toRTypeRep sort


checkDefAsserts :: BareEnv -> [(Var, LocSymbol, BareType)] -> [(LocSymbol, BareType)] -> BareM ()
checkDefAsserts env vbs xbs   = applyNonNull (return ()) grumble  undefSigs
  where
    undefSigs                 = [x | (x, _) <- assertSigs, not (x `S.member` definedSigs)]
    assertSigs                = filter isTarget xbs
    definedSigs               = S.fromList $ snd3 <$> vbs
    grumble                   = mapM_ (warn . berrUnknownVar)
    moduleName                = symbol $ modName env
    isTarget                  = isPrefixOfSym moduleName . stripParensSym . val . fst

warn x = tell [x]

-------------------------------------------------------------------------------------
-- | Tasteful Error Messages --------------------------------------------------------
-------------------------------------------------------------------------------------

berrUnknownVar       = berrUnknown "Variable"

berrUnknown :: (PPrint a) => String -> Located a -> String 
berrUnknown thing x  = printf "[%s]\nSpecification for unknown %s : %s"  
                         thing (showpp $ loc x) (showpp $ val x)

