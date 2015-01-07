{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.Bare.Type.Alias (
    
  ) where

import Control.Arrow (second)

import qualified Control.Exception   as Ex
import qualified Data.HashMap.Strict as M

import Language.Fixpoint.Misc (errorstar)

import Language.Haskell.Liquid.RefType (symbolRTyVar)

import Language.Haskell.Liquid.Bare.Env (Out)

import Language.Haskell.Liquid.Bare.Type.Make

--------------------------------------------------------------------------------

type AliasM t = StateT (M.HashMap Symbol (RTAlias Symbol t)) Out

makeRTAliases xts
  = do let table = buildAliasTable xts
           graph = buildAliasGraph buildTypeEdges $ map snd xts
       checkCyclicAliases table graph

       mapM_ expBody $ genExpandOrder table graph
  where
    expBody (mod, xt)
      = do typeAliases <- get
           body <- makeType typeAliases xt
           modify $ M.insert (rtName xt) $ mapRTAVars symbolRTyVar $ xt { rtBody = body }

--------------------------------------------------------------------------------

type AliasTable t = M.HashMap Symbol (ModName, RTAlias Symbol t)

buildAliasTable :: [(ModName, RTAlias Symbol t)] -> AliasTable t
buildAliasTable
  = M.fromList . map (\(mod, rta) -> (rtName rta, (mod, rta)))

fromAliasSymbol :: AliasTable t -> Symbol -> (ModName, RTAlias Symbol BareType)
fromAliasSymbol table sym
  = fromMaybe err $ M.lookup sym table
  where
    err = errostart $ "fromAliasSymbol: Dangling alias symbol: " ++ show sym


type Graph t = [Node t]
type Node  t = (t, t, [t])

buildAliasGraph :: (t -> [Symbol]) -> [RTAlias Symbol t] -> Graph Symbol
buildAliasGraph buildEdges
  = map (buildAliasNode buildEdges)

buildAliasNode :: (t -> [Symbol]) -> RTAlias Symbol t -> Node Symbol
buildAliasNode buildEdges alias
  = (rtName alias, rtName alias, buildEdges $ rtBody alias)


checkCyclicAliases :: AliasTable t -> Graph Symbol -> Out ()
checkCyclicAliases table graph
  = case mapMaybe go $ stronglyConnComp graph of
      [] ->
        return ()
      sccs ->
        Ex.throw $ map err sccs
  where
    go (AcyclicSCC _)
      = Nothing
    go (CyclicSCC vs)
      = Just vs

    err :: [Symbol] -> Error
    err scc
      = ErrAliasCycle { pos   = fst $ locate rta
                      , acycle = map locate scc
                      }

    locate sym
      = (sourcePosSrcSpan $ rtPos alias, pprint sym)
      where (_, alias) = fromAliasSymbol table sym


genExpandOrder :: AliasTable t -> Graph Symbol -> [(ModName, RTAlias Symbol t)]
genExpandOrder table graph
  = map (fromAliasSymbol table) symOrder
  where
    (digraph, lookupVertex, _)
      = graphFromEdges graph
    symOrder
      = map (fst3 . lookupVertex) $ reverse $ topSort digraph

--------------------------------------------------------------------------------

buildTypeEdges :: AliasTable t -> BareType -> [Symbol]
buildTypeEdges table
  = ordNub . go
  where go (RApp (Loc _ c) ts rs _)
          = curr ++ concat (map go ts ++ map go (mapMaybe ref rs))
          where
            curr = case M.lookup c table of
                     Nothing -> [ ]
                     Just _  -> [c]

            ref (RPropP _ _) = Nothing
            ref (RProp  _ t) = Just t
            ref (RHProp _ _) = errorstar "TODO:EFFECTS:buidTypeEdges"

        go (RRTy e _ _ t)
          = concat (map (second go) e) ++ go t

        go (RFun _ t1 t2 _)
          = go t1 ++ go t2
        go (RAppTy t1 t2 _)
          = go t1 ++ go t2
        go (RAllE _ t1 t2)
          = go t1 ++ go t2
        go (REx _ t1 t2)
          = go t1 ++ go t2
        go (RAllT _ t)
          = go t
        go (RAllP _ t)
          = go t
        go (RAllS _ t)
          = go t

        go (RVar _ _)
          = []
        go (ROth _)
          = []
        go (RExprArg _)
          = []
        go (RHole _)
          = []

