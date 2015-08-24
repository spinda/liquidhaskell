module Language.Haskell.Liquid.TH.Parse.Dec (
    decP
  ) where

import Control.Monad
import Control.Monad.Trans

import Data.List
import Data.Maybe

import Language.Haskell.TH.Syntax hiding (Loc)

import Text.Parsec
import Text.Parsec.Combinator

import Language.Fixpoint.Types

import Language.Haskell.Liquid.RefType (quantifyFreeRTy)
import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.TH.Misc
import Language.Haskell.Liquid.TH.Simplify
import Language.Haskell.Liquid.TH.Types

import Language.Haskell.Liquid.TH.Parse.Base
import Language.Haskell.Liquid.TH.Parse.Type

--------------------------------------------------------------------------------
-- Top-Level Entry Point -------------------------------------------------------
--------------------------------------------------------------------------------

decP :: Parser [Dec]
decP = choice
  [ embedP
  , logicP
--  , dataP
  , tySynP
  , fnSigP
  ]

--------------------------------------------------------------------------------
-- FTycon Embed Annotations ----------------------------------------------------
--------------------------------------------------------------------------------

embedP :: Parser [Dec]
embedP = do
  (s, tc) <- reserved "embed" *> withSpan (tyConP <|> tyConOpP <|> parens tyConOpP)
  fc      <- reserved "as"    *> located fTyconP
  tc'     <- liftP $ fromMaybe (mkName tc) <$> lookupTypeName tc
  ann     <- dataToExpP $ EmbedAs fc s
  ifSimplified [] [PragmaD $ AnnP (TypeAnnotation tc') ann]

--------------------------------------------------------------------------------
-- Inline, Bound, and Measure Annotations --------------------------------------
--------------------------------------------------------------------------------

logicP :: Parser [Dec]
logicP = do
  k       <- modifier logicKindP
  assumed <- optionBool $ modifier $ reserved "assume"
  (s, v)  <- withSpan $ mkName <$> varidP
  sig     <- option [] $ fnSigP' s v assumed
  ann     <- dataToExpP $ LiftToLogic k s
  ifSimplified sig (PragmaD (AnnP (ValueAnnotation v) ann) : sig)

logicKindP :: Parser LogicKind
logicKindP = (InlineKind  <$ reserved "inline")
         <|> (BoundKind   <$ reserved "bound")
         <|> (MeasureKind <$ reserved "measure")

--------------------------------------------------------------------------------
-- Data Declarations -----------------------------------------------------------
--------------------------------------------------------------------------------
 
{-
dataP :: Parser [Dec]
dataP = do
  (cxt, tc, tvs) <- reserved   "data" *> dataHeadP
  cons           <- reservedOp "="    *> sepBy1 dataConP (reservedOp "|")
  drv            <- option [] $ reserved "deriving" *> dataDerivP

dataHeadP :: Parser ([AnnoType], Name, [String])
dataHeadP = (go =<< typeP) <?> "data declaration head"
  where
    go (Loc p _ ty)
      | isTrivial ty
      , not $ isWiredInName $ val c
      , all isRVar ts
      , (cxt, RApp c ts [] _) <- splitCxtArrowRTy ty =
        return (cxt, val c, rt_var <$> ts)
      | otherwise = failAt p "malformed data declaration head"

dataConP ::

dataDerivP :: Parser [(Name, [AnnoType])]
dataDerivP = (go =<< typeP) <?> "data deriving list"
  where
    go (Loc p _ ty)
      | not $ isTrivial ty = malformed p
      | otherwise          = mapM (go' p) (splitTuplesRTy ty)

    go' _ (RApp c ts [] _) = return (c, ts)
    go' p _                = malformed p

    malformed p = failAt p " malformed data deriving list"
-}

--------------------------------------------------------------------------------
-- Type Synonym Declarations ---------------------------------------------------
--------------------------------------------------------------------------------

tySynP :: Parser [Dec]
tySynP = do
  (s, con) <- reserved "type" *> withSpan ((liftP . newName) =<< tyConP)
  tvs      <- map (PlainTV . mkName) <$> many tyVarP
  evs      <- exprParamsP
  rhs      <- reservedOp "=" *> withExprParams evs typeP
  let decl  = TySynD con tvs $ simplifyRTy $ val rhs
  ann      <- dataToExpP $ LiquidSyn evs rhs s
  ifSimplified [decl] [PragmaD $ AnnP (TypeAnnotation con) ann, decl]

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
fnSigP = do
  assumed  <- optionBool $ modifier $ reserved "assume"
  (s, var) <- withSpan $ mkName <$> (parens operator <|> varidP)
  fnSigP' s var assumed

fnSigP' :: SourceSpan -> Name -> Bool -> Parser [Dec]
fnSigP' s var assumed = reservedOp "::" *> do
  ty       <- fmap quantifyFreeRTy <$> typeP
  let decl  = SigD var $ simplifyRTy $ val ty
  ann      <- dataToExpP $ LiquidVar True assumed ty s
  ifSimplified [decl] [PragmaD $ AnnP (ValueAnnotation var) ann, decl]

--------------------------------------------------------------------------------
-- Utility Functions -----------------------------------------------------------
--------------------------------------------------------------------------------

modifier :: Parser a -> Parser a
modifier p = try $ p <* notFollowedBy (reservedOp "::")

--------------------------------------------------------------------------------
-- Error Messages --------------------------------------------------------------
--------------------------------------------------------------------------------

errExprParamTail :: String
errExprParamTail =
  "Type variables cannot follow expression parameters: expression parameters must be at the end"

errDupExprParam :: String -> String
errDupExprParam param =
  "Duplicate expression parameter \"" ++ param ++ "\""

