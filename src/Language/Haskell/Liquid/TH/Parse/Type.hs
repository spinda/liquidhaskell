{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.TH.Parse.Type (
    -- * Parse Refinement Types 
    typeP
  ) where

import Control.Monad
import Control.Monad.State

import Data.List
import Data.Maybe

import qualified Data.HashSet as S

import Language.Haskell.TH.Syntax hiding (Infix, Loc, lift)

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Expr

import Language.Fixpoint.Types

import Language.Haskell.Liquid.RefType (quantifyRTy, strengthen)
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.TH.Types
import Language.Haskell.Liquid.TH.WiredIns

import Language.Haskell.Liquid.TH.Parse.Base
import Language.Haskell.Liquid.TH.Parse.Logic

--------------------------------------------------------------------------------
-- Parse Refinement Types ------------------------------------------------------
--------------------------------------------------------------------------------

typeP :: Parser (Located AnnoType)
typeP = named "type" $ do
  t <- located typeP'
  t <$ checkType (val t)

typeP' :: Parser AnnoType
typeP' = forallTypeP <|> innerTypeP

forallTypeP :: Parser AnnoType
forallTypeP = do
  reserved "forall"
  tvs <- many tyVarP
  reservedOp "."
  ty  <- typeP'
  return $ quantifyRTy tvs ty

innerTypeP :: Parser AnnoType
innerTypeP = do
  b <- optionMaybe $ try (located binderP <* colon)
  t <- located argTyP
  appTyP b t

--------------------------------------------------------------------------------
-- Parse Type Argument ---------------------------------------------------------
--------------------------------------------------------------------------------

argTyP :: Parser AnnoType
argTyP = do
  (t:ts) <- many1 termTyP
  return $ case t of
    RApp c as ps r
      | val c == funArrowTcName, [t1, t2] <- as ++ ts ->
        RFun dummySymbol t1 t2 mempty
      | otherwise ->
        RApp c (as ++ ts) ps r
    _ -> foldl' (\x y -> RAppTy x y mempty) t ts

termTyP :: Parser AnnoType
termTyP = parensTyP
      <|> bracesTyP
      <|> listTyP
      <|> varTyP
      <|> conTyP


parensTyP :: Parser AnnoType
parensTyP = try opTyP <|> tupTyP

opTyP :: Parser AnnoType
opTyP = parens $ do
  op <- located $ qual tyConOpP
  return $ RApp (mkName <$> op) [] [] mempty

tupTyP :: Parser AnnoType
tupTyP = do
  Loc s e tys <- located $ parens $ sepBy typeP' comma
  return $ case tys of
    [ty] -> ty
    _    -> RApp (Loc s e tupleTcName) tys [] mempty


bracesTyP :: Parser AnnoType
bracesTyP = braces $ refinedTyP <|> exprTyP

refinedTyP :: Parser AnnoType
refinedTyP = do
  b <- try (symbol <$> binderP <* colon)
  t <- typeP'
  p <- reservedOp "|" *> predP
  let r = Reft (b, Refa p)
  let u = (mempty :: RReft) { ur_reft = r }
  return $ t `strengthen` u

exprTyP :: Parser AnnoType
exprTyP = do
  e <- located exprP
  return $ RExprArg e


listTyP :: Parser AnnoType
listTyP = do
  Loc s e t <- located $ brackets typeP'
  return $ RApp (Loc s e listTcName) [t] [] mempty


varTyP :: Parser AnnoType
varTyP = (`RVar` mempty) <$> tyVarP

conTyP :: Parser AnnoType
conTyP = do
  con <- located $ qual $ tyConP <|> (lookAhead tyConOpP >> parseFail errTyConOp)
  ep  <- isExprParam $ val con
  return $ if ep
    then RExprArg $ EVar . symbol <$> con
    else RApp (mkName <$> con) [] [] mempty

--------------------------------------------------------------------------------
-- Apply Type Argument ---------------------------------------------------------
--------------------------------------------------------------------------------

appTyP :: Maybe (Located String) -> Located AnnoType -> Parser AnnoType
appTyP b t = funTyP b t
         <|> cxtTyP b t
         <|> eqTyP  b t
         <|> resTyP b t


funTyP :: Maybe (Located String) -> Located AnnoType -> Parser AnnoType
funTyP b t1 = do
  t2 <- reservedOp "->" *> typeP'
  let b' = fromMaybe dummySymbol $ symbol . val <$> b
  return $ RFun b' (val t1) t2 mempty

cxtTyP :: Maybe (Located String) -> Located AnnoType -> Parser AnnoType
cxtTyP (Just b) _ = failAt (loc b) errBindPos
cxtTyP Nothing  c = do
  a <- located (cxtArrowTcName <$ reservedOp "=>")
  t <- typeP'
  return $ RApp a [val c, t] [] mempty

eqTyP :: Maybe (Located String) -> Located AnnoType -> Parser AnnoType
eqTyP (Just b) _  = failAt (loc b) errBindPos
eqTyP Nothing  t1 = do
  eq <- located (equalityTcName <$ reservedOp "~")
  t2 <- typeP'
  return $ RApp eq [val t1, t2] [] mempty

resTyP :: Maybe (Located String) -> Located AnnoType -> Parser AnnoType
resTyP (Just b) _ = failAt (loc b) errBindPos
resTyP Nothing  t = return $ val t

--------------------------------------------------------------------------------
-- Check Parsed Type -----------------------------------------------------------
--------------------------------------------------------------------------------

checkType :: AnnoType -> Parser ()
checkType (RVar     _ _    ) = return ()
checkType (RFun     _ i o _) = checkType i >> checkType o
checkType (RAllT    _ t    ) = checkType t
checkType (RAllP    _ t    ) = checkType t
checkType (RAllS    _ t    ) = checkType t
checkType (RApp     _ a _ _) = checkRApp a
checkType (RAllE    _ _ t  ) = checkType t
checkType (REx      _ _ t  ) = checkType t
checkType (RExprArg e      ) = failAt (loc e) errExprArgPos
checkType (RAppTy   x y _  ) = checkType x >> checkType y
checkType (RRTy     _ _ _ t) = checkType t
checkType (RHole    _      ) = return ()

checkRApp :: [AnnoType] -> Parser ()
checkRApp []       = return ()
checkRApp (a : as) = case a of
  RExprArg e | all isExprArg as -> return ()
             | otherwise        -> failAt (loc e) errExprArgPos
  _                             -> checkRApp as

--------------------------------------------------------------------------------
-- Error Messages --------------------------------------------------------------
--------------------------------------------------------------------------------

errTyConOp :: String
errTyConOp =
  "type constructor operators must be surrounded in parentheses"

errBindPos :: String
errBindPos =
  "binder cannot appear at this location"

errExprArgPos :: String
errExprArgPos =
  "expression arguments must appear at the end of a type constructor application"

