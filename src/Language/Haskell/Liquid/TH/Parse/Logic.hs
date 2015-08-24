{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.Liquid.TH.Parse.Logic (
    predP
  , exprP
  ) where

import Control.DeepSeq
import Control.Exception as E

import Data.Function
import Data.Typeable (Typeable)

import Text.Parsec hiding (Pred)
import Text.Parsec.Combinator
import Text.Parsec.Expr

import Language.Haskell.TH.Syntax (runIO)

import Language.Fixpoint.Types hiding (pAnd, pOr)

import Language.Haskell.Liquid.TH.Parse.Base

--------------------------------------------------------------------------------
-- Top-Level Entry Points ------------------------------------------------------
--------------------------------------------------------------------------------

predP :: Parser Pred
predP = (asPred <$> logicP) <?> "logical predicate"

exprP :: Parser Expr
exprP = (asExprP =<< logicP) <?> "logical expression"

--------------------------------------------------------------------------------
-- Logic Representation and Conversion -----------------------------------------
--------------------------------------------------------------------------------

type Logic = (SourcePos, Either Pred Expr)

data LogicError = ErrPredAsExpr SourcePos deriving (Show, Typeable)

instance Exception LogicError

asPred :: Logic -> Pred
asPred (_, Left  p) = p
asPred (_, Right e) = PBexp e

asExpr :: Logic -> Expr
asExpr (_, Left (PBexp e)) = e
asExpr (_, Right e)        = e
asExpr (s, _)              = throw $ ErrPredAsExpr s

asExprP :: Logic -> Parser Expr
asExprP (_, Left (PBexp e)) = return e
asExprP (_, Right e)        = return e
asExprP (s, _)              = failAt s "unexpected predicate\nexpecting expression"

--------------------------------------------------------------------------------
-- Inner Logic Parser ----------------------------------------------------------
--------------------------------------------------------------------------------

logicP :: Parser Logic
logicP = named "logic" $ do
  (s, l) <- buildExpressionParser lops ltermP 
  result <- liftP $ runIO $ E.try $ return $!! l
  case result of
    Left  (ErrPredAsExpr s) -> failAt s "unexpected predicate\nexpecting expression"
    Right l'                -> return (s, l')

-- Logic Term Parsing ----------------------------------------------------------

ltermP :: Parser Logic
ltermP = withPos $ do
  l  <- located ltermP'
  ls <- many $ withPos ltermP'
  case ls of
    [] -> return $ val l
    _  -> do
      e  <- asExprP (loc l, val l)
      es <- mapM asExprP ls
      case e of
        EVar v     -> return $ Right $ EApp (const v <$> l) es
        EApp f es' -> return $ Right $ EApp f (es' ++ es)
        otherwise  -> failAt (loc l) "invalid expression application"

ltermP' :: Parser (Either Pred Expr)
ltermP' = (snd <$> parens logicP)
      <|> (Left  PTrue  <$ reserved   "true")
      <|> (Left  PFalse <$ reserved   "false")
      <|> (Right EBot   <$ reservedOp "_|_")
      <|> iteP
      <|> (Right . ECon . I <$> natural)
      <|> (Right <$> eVarP)


iteP :: Parser (Either Pred Expr)
iteP = do
  p  <- reserved "if"   *> (asPred <$> logicP)
  b1 <- reserved "then" *> logicP
  b2 <- reserved "else" *> logicP
  case snd b1 of
    Left  p1 -> return $ Left $ pIte p p1 $ asPred b2
    Right e1 -> Right . EIte p e1 <$> asExprP b2

eVarP :: Parser Expr
eVarP = do
  name <- qual $ choice
    [ binderP <?> "variable or binder"
    , conidP  <?> "data constructor or expression parameter"
    ]
  return $ EVar $ symbol name

-- Logic Operator Parsing ------------------------------------------------------

lops = [ [ prefixOp Right (ENeg          . asExpr) (reservedOp "-"  )
         ]
       , [ infixOp  Right (EBin Times `on` asExpr) (reservedOp "*"  ) AssocLeft
         , infixOp  Right (EBin Div   `on` asExpr) (reservedOp "/"  ) AssocLeft
         ]
       , [ infixOp  Right (EBin Minus `on` asExpr) (reservedOp "-"  ) AssocLeft
         , infixOp  Right (EBin Plus  `on` asExpr) (reservedOp "+"  ) AssocLeft
         ]
       , [ infixOp  Right (EBin Mod   `on` asExpr) (reserved   "mod") AssocLeft
         ]
       , [ infixOp  Left  (PAtom Eq   `on` asExpr) (reservedOp "==" ) AssocRight
         , infixOp  Left  (PAtom Eq   `on` asExpr) (reservedOp "="  ) AssocRight
         , infixOp  Left  (PAtom Ne   `on` asExpr) (reservedOp "!=" ) AssocRight
         , infixOp  Left  (PAtom Ne   `on` asExpr) (reservedOp "/=" ) AssocRight
         , infixOp  Left  (PAtom Ueq  `on` asExpr) (reservedOp "~~" ) AssocRight
         , infixOp  Left  (PAtom Une  `on` asExpr) (reservedOp "!~" ) AssocRight
         , infixOp  Left  (PAtom Lt   `on` asExpr) (reservedOp "<"  ) AssocRight
         , infixOp  Left  (PAtom Le   `on` asExpr) (reservedOp "<=" ) AssocRight
         , infixOp  Left  (PAtom Gt   `on` asExpr) (reservedOp ">"  ) AssocRight
         , infixOp  Left  (PAtom Ge   `on` asExpr) (reservedOp ">=" ) AssocRight
         ]
       , [ prefixOp Left  (PNot          . asPred) (reservedOp "~"  )
         , prefixOp Left  (PNot          . asPred) (reserved   "not")
         ]
       , [ infixOp  Left  (pAnd       `on` asPred) (reservedOp "&&" ) AssocRight
         ]
       , [ infixOp  Left  (pOr        `on` asPred) (reservedOp "||" ) AssocRight
         ]
       , [ infixOp  Left  (PImp       `on` asPred) (reservedOp "=>" ) AssocRight
         , infixOp  Left  (PImp       `on` asPred) (reservedOp "==>") AssocRight
         ]
       , [ infixOp  Left  (PIff       `on` asPred) (reservedOp "<=>") AssocRight
         ]
       ]


prefixOp c f p = Prefix $ do
  s <- position
  p
  return $ \x -> (s, c $ f x)

infixOp c f p a = (`Infix` a) $ do
  p
  return $ \x y -> (fst x, c $ f x y)


pAnd :: Pred -> Pred -> Pred
pAnd p1 (PAnd ps) = PAnd (p1:ps)
pAnd p1 p2        = PAnd [p1, p2]

pOr :: Pred -> Pred -> Pred
pOr p1 (POr ps) = POr (p1:ps)
pOr p1 p2       = POr [p1, p2]

