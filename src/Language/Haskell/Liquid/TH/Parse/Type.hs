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

import Language.Haskell.TH.Syntax hiding (Infix, lift)

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Expr

import Language.Haskell.Liquid.TH.Encode

import Language.Haskell.Liquid.TH.Parse.Base
import Language.Haskell.Liquid.TH.Parse.Reft

--------------------------------------------------------------------------------
-- Parse Refinement Types ------------------------------------------------------
--------------------------------------------------------------------------------

typeP :: Parser ([Name], Type)
typeP = named "type" $ collectFreeTyVars typeP'


typeP' :: Parser Type
typeP' = buildExpressionParser tops ttermP

tops = [ [Infix ((\x y -> ArrowT    `AppT` x `AppT` y) <$ reservedOp "->") AssocRight]
       , [Infix (ForallT [] . return                   <$ reservedOp "=>") AssocRight]
       , [Infix ((\x y -> EqualityT `AppT` x `AppT` y) <$ reservedOp "~" ) AssocLeft ]
       ]


ttermP :: Parser Type
ttermP = do
  b  <- optionMaybe $ try (located binderP <* colon)
  notExpecting "expression argument (must appear at end of type application)" exprArgP
  t  <- ttermInnerP
  es <- many exprArgP
  notExpecting "type arguments after expression arguments" ttermInnerP
  ifSimplified t $
    let t_es   = if null es then t else exprArgs es t
        b_t_es = maybe t_es (`bind` t_es) b
    in  b_t_es

ttermInnerP :: Parser Type
ttermInnerP = (refinedTyP <* (notExpecting "application to refined type" ttermInnerP))
          <|> appTyP


refinedTyP :: Parser Type
refinedTyP = named "refined type" $ do
  b <- try (reservedOp "{" *> binderP <* colon)
  t <- typeP'
  r <- reservedOp "|" *> reftP <* reservedOp "}"
  ifSimplified t $ refine b r t


appTyP :: Parser Type
appTyP = foldl1' AppT <$> many1 unitTyP

unitTyP :: Parser Type
unitTyP = listTyP
      <|> parensTyP
      <|> refinedTyP
      <|> varTyP
      <|> conTyP


listTyP :: Parser Type
listTyP = AppT ListT <$> brackets typeP'

parensTyP :: Parser Type
parensTyP = parens $ try opTyP <|> do
  tys <- sepBy typeP' comma
  return $ case tys of
    [ty] -> ty
    _    -> foldl' AppT (TupleT $ length tys) tys

opTyP :: Parser Type
opTyP = named "type operator" $
      (ArrowT        <$  reservedOp "->")
  <|> (EqualityT     <$  reservedOp "~")
  <|> (ConT . mkName <$> qual tyConOpP)

conTyP :: Parser Type
conTyP = try $ do
  c  <- qual tyConP <?> "type constructor"
  ep <- isExprParam c
  if ep then mzero else return $ ConT $ mkName c

varTyP :: Parser Type
varTyP = named "type variable" $ do
  tv <- tyVarP
  visitTyVar tv
  return $ VarT $ mkName tv


exprArgP :: Parser (Located Expr)
exprArgP = named "expression argument" $ located $
  braces exprP <|> try plainParam
  where
    plainParam = do
      param <- exprParamP
      ep    <- isExprParam param
      if ep then return $ eParam param else mzero

