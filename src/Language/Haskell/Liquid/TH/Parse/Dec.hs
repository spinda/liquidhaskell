module Language.Haskell.Liquid.TH.Parse.Dec (
    decP
  ) where

import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import Language.Haskell.TH.Syntax

import Text.Parsec
import Text.Parsec.Combinator

import Language.Haskell.Liquid.TH.Encode

import Language.Haskell.Liquid.TH.Parse.Base
import Language.Haskell.Liquid.TH.Parse.Type

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

decP :: Parser [Dec]
decP = choice
  [ embedP
  , inlineP
  , boundP
  , measureP
  , aliasP
  , tySynP
  , fnSigP
  ]

--------------------------------------------------------------------------------
-- FTycon Embed Annotations ----------------------------------------------------
--------------------------------------------------------------------------------

embedP :: Parser [Dec]
embedP = do
  tc  <- reserved "embed" *> (tyConP <|> tyConOpP <|> parens tyConOpP)
  fc  <- reserved "as"    *> located fTyconP
  tc' <- liftP $ fromMaybe (mkName tc) <$> lookupTypeName tc
  ifSimplified [] [annEmbedAs tc' fc]

--------------------------------------------------------------------------------
-- Inline, Bound, and Measure Annotations --------------------------------------
--------------------------------------------------------------------------------

-- TODO: Handle cases where these are function names, not keywords

inlineP :: Parser [Dec]
inlineP = do
  var <- reserved "inline" *> located (mkName <$> varidP)
  sig <- option [] $ fnSigP' $ val var
  ifSimplified sig ([annIsInline var Nothing] ++ sig)

boundP :: Parser [Dec]
boundP = do
  var <- reserved "bound" *> located (mkName <$> varidP)
  sig <- option [] $ fnSigP' $ val var
  ifSimplified sig ([annIsBound var Nothing] ++ sig)

measureP :: Parser [Dec]
measureP = do
  var <- reserved "measure" *> located (mkName <$> varidP)
  sig <- option [] $ fnSigP' $ val var
  ifSimplified sig ([annIsMeasure var Nothing] ++ sig)


aliasP :: Parser [Dec]
aliasP = do
  ann <- reserved "alias" *> kind
  var <- located (mkName <$> varidP)
  tgt <- reservedOp "=" *> located varidP
  ifSimplified [] [ann var $ Just tgt]
  where
    kind = annIsInline  <$ reserved "inline"
       <|> annIsBound   <$ reserved "bound"
       <|> annIsMeasure <$ reserved "measure"

--------------------------------------------------------------------------------
-- Type Synonym Declarations ---------------------------------------------------
--------------------------------------------------------------------------------

tySynP :: Parser [Dec]
tySynP = named "type synonym" $ do
  con       <- (liftP . newName) =<< reserved "type" *> tyConP
  tvs       <- map (PlainTV . mkName) <$> many tyVarP
  evs       <- located exprParamsP
  (_, ty)   <- reservedOp "=" *> withExprParams (val evs) typeP
  let tySynD = TySynD con tvs ty
  ifSimplified [tySynD] [tySynD, annExprParams con evs]

exprParamsP :: Parser [String]
exprParamsP = do
  evs <- foldM checkUnique [] =<< many (withPos exprParamP)
  noParse errExprParamTail tyVarP
  return evs
  where
    checkUnique seen (p, param)
      | param `elem` seen = failAt p $ errDupExprParam param
      | otherwise         = return (param:seen)

--------------------------------------------------------------------------------
-- Function Signature Declarations ---------------------------------------------
--------------------------------------------------------------------------------

fnSigP :: Parser [Dec]
fnSigP = named "signature" $ fnSigP' =<< (mkName <$> varidP)

fnSigP' :: Name -> Parser [Dec]
fnSigP' var = named "signature" $
  reservedOp "::" *> fmap (return . SigD var . quantify) typeP
  where
    quantify (tvs, ty) = ForallT (map PlainTV tvs) [] ty

--------------------------------------------------------------------------------
-- Error Messages --------------------------------------------------------------
--------------------------------------------------------------------------------

errExprParamTail :: String
errExprParamTail =
  "Type variables cannot follow expression parameters: expression parameters must be at the end"

errDupExprParam :: String -> String
errDupExprParam param =
  "Duplicate expression parameter \"" ++ param ++ "\""

