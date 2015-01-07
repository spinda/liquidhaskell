module Language.Haskell.Liquid.Bare.Type.Expand (
    failRTAliasApp
  , expandRTAliasApp
  ) where

-- TODO: Condense this a bit
failRTAliasApp :: BRType r -> RTAlias RTyVar SpecType -> TypeM r (RRType r)
failRTAliasApp (RApp (Loc l _) _ _ _) rta
  = Ex.throw err
  where
    err :: Error
    err = ErrIllegalAliasApp (sourcePosSrcSpan l) (pprint $ rtName rta) (sourcePosSrcSpan $ rtPos rta)
failRTAliasApp _ _
  = errorstar "Bare.Type.failRTAliasApp called with invalid input"

-- TODO: Why aren't pargs handled here? Should they be?
expandRTAliasApp :: BareType -> RTAlias RTyVar SpecType -> TypeM RReft SpecType
expandRTAliasApp (RApp (Loc l _) ts _ r) rta
  = do r'  <- resolveReft r
       env <- ask
       lift $ withVArgs l (rtVArgs rta) $ runReaderT (expandRTAliasApp' l rta ts r') env
expandRTAliasApp _ _
  = errorstar "Bare.Type.expandRTAliasApp called with invalid input"

expandRTAliasApp' :: SourcePos -> RTAlias RTyVar SpecType -> [BareType] -> RReft -> TypeM RReft SpecType
expandRTAliasApp' l rta args r
  | length args == length αs + length εs
    = do args' <- mapM ofBRType' args
         let ts  = take (length αs) args'
             αts = zipWith (\α t -> (α, toRSort t, t)) αs ts
         return $ subst su . (`strengthen` r) . subsTyVars_meet αts $ rtBody rta
  | otherwise
    = Ex.throw err
  where
    su        = mkSubst $ zip (symbol <$> εs) es
    αs        = rtTArgs rta 
    εs        = rtVArgs rta
    es_       = drop (length αs) args
    es        = map (exprArg $ show err) es_
    err       :: Error
    err       = ErrAliasApp (sourcePosSrcSpan l) (length args) (pprint $ rtName rta) (sourcePosSrcSpan $ rtPos rta) (length αs + length εs) 

-- | exprArg converts a tyVar to an exprVar because parser cannot tell 
-- HORRIBLE HACK To allow treating upperCase X as value variables X
-- e.g. type Matrix a Row Col = List (List a Row) Col

exprArg _   (RExprArg e)     
  = e
exprArg _   (RVar x _)       
  = EVar (symbol x)
exprArg _   (RApp x [] [] _) 
  = EVar (symbol x)
exprArg msg (RApp f ts [] _) 
  = EApp (symbol <$> f) (exprArg msg <$> ts)
exprArg msg (RAppTy (RVar f _) t _)   
  = EApp (dummyLoc $ symbol f) [exprArg msg t]
exprArg msg z 
  = errorstar $ printf "Unexpected expression parameter: %s in %s" (show z) msg 

