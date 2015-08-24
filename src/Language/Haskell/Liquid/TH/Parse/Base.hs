{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.TH.Parse.Base (
    module Language.Haskell.Liquid.Parse

    -- * Parser Type
  , Parser
  , runParser

    -- * Position Information
  , withSpan

    -- * Simplified Output
  , getSimplified
  , ifSimplified

    -- * Expression Parameters
  , withExprParams
  , isExprParam

    -- * Convert Data to Exp
  , dataToExpP

    -- * Identifier Parsers
  , fTyconP
  ) where

import Control.Monad.Identity

import Data.Data
import Data.List
import Data.Maybe

import qualified Data.HashSet as S

import Language.Haskell.TH.Syntax

import Text.Parsec hiding (runParser, runParserT)
import Text.Parsec.Pos

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Parse

import Language.Haskell.Liquid.TH.Misc
import Language.Haskell.Liquid.TH.Types

--------------------------------------------------------------------------------
-- Parser Type -----------------------------------------------------------------
--------------------------------------------------------------------------------

type Parser = ParserT ParserState Q

data ParserState = PS { ps_simplified :: Bool
                      , ps_exprParams :: S.HashSet String
                      }

runParser :: Bool -> Parser a -> String -> Q a
runParser simplified p src = do
  loc    <- location
  result <- runParserT p (PS simplified mempty) (locSourcePos loc) src
  case result of
    Left  err -> failWithError (loc_filename loc) err
    Right out -> return out

locSourcePos :: Loc -> SourcePos
locSourcePos loc =
  newPos (loc_filename loc) (fst (loc_start loc) - 1) (snd (loc_start loc) - 1)

failWithError :: FilePath -> ParseError -> Q a
failWithError file err = do
  src <- runIO $ readFile file
  fail $ '\n' : indent (formatError src err)
  where
    indent = unlines . map ("    "++) . lines

--------------------------------------------------------------------------------
-- Position Information --------------------------------------------------------
--------------------------------------------------------------------------------

withSpan :: Parser a -> Parser (SourceSpan, a)
withSpan p = do
  x <- located p
  return (locatedSpan x, val x)

--------------------------------------------------------------------------------
-- Simplified Output -----------------------------------------------------------
--------------------------------------------------------------------------------

getSimplified :: Parser Bool
getSimplified = ps_simplified <$> getP

ifSimplified :: a -> a -> Parser a
ifSimplified x y = do
  simplified <- getSimplified
  return $ if simplified
    then x
    else y

--------------------------------------------------------------------------------
-- Expression Parameters -------------------------------------------------------
--------------------------------------------------------------------------------

withExprParams :: [String] -> Parser a -> Parser a
withExprParams params act = do
  paramSet <- ps_exprParams <$> getP
  modifyP $ \ps -> ps { ps_exprParams = S.union (S.fromList params) paramSet }
  result <- act
  modifyP $ \ps -> ps { ps_exprParams = paramSet }
  return result

isExprParam :: String -> Parser Bool
isExprParam param = do
  params <- ps_exprParams <$> getP
  return (param `S.member` params)

--------------------------------------------------------------------------------
-- Convert Data to Exp ---------------------------------------------------------
--------------------------------------------------------------------------------

dataToExpP :: Data a => a -> Parser Exp
dataToExpP = liftP . dataToExpQ'

--------------------------------------------------------------------------------
-- Identifier Parsers ----------------------------------------------------------
--------------------------------------------------------------------------------

fTyconP :: Parser FTycon
fTyconP = (intFTyCon  <$ reserved "int")
      <|> (intFTyCon  <$ reserved "Integer")
      <|> (intFTyCon  <$ reserved "Int")
      <|> (intFTyCon  <$ reserved "int")
      <|> (realFTyCon <$ reserved "real")
      <|> (boolFTyCon <$ reserved "bool")
      <|> (symbolFTycon . fmap symbol <$> located conidP)

