{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.TH.Parse.Base (
    module Language.Haskell.Liquid.Parse

    -- * Parser Type
  , Parser
  , runParser

    -- * Position Information
  , located

    -- * Simplified Output
  , getSimplified
  , ifSimplified

    -- * Expression Parameters
  , withExprParams
  , isExprParam

    -- * Free Type Variables
  , collectFreeTyVars
  , visitTyVar

    -- * Identifier Parsers
  , fTyconP
  ) where

import Data.List
import Data.Maybe

import qualified Data.HashSet as S

import Language.Haskell.TH.Syntax

import Text.Parsec hiding (runParser, runParserT)
import Text.Parsec.Pos

import Language.Haskell.Liquid.Parse

import Language.Haskell.Liquid.TH.Encode
import Language.Haskell.Liquid.TH.Types

--------------------------------------------------------------------------------
-- Parser Type -----------------------------------------------------------------
--------------------------------------------------------------------------------

type Parser = ParserT ParserState Q

data ParserState = PS { ps_simplified :: Bool
                      , ps_exprParams :: S.HashSet String
                      , ps_freeTyVars :: Maybe (S.HashSet String)
                      }

runParser :: Bool -> Parser a -> String -> Q a
runParser simplified p src = do
  loc    <- location
  result <- runParserT p (PS simplified mempty Nothing) (locSourcePos loc) src
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

located :: Monad m => ParserT s m a -> ParserT s m (Located a)
located p = do
  s <- position
  a <- p
  e <- position -- TODO: End position minus whitespace!
  return $ mkLocated s e a

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
  ps <- getP
  putP $ ps { ps_exprParams = S.union (S.fromList params) (ps_exprParams ps) }
  result <- act
  putP ps
  return result

isExprParam :: String -> Parser Bool
isExprParam param = do
  params <- ps_exprParams <$> getP
  return (param `S.member` params)

--------------------------------------------------------------------------------
-- Free Type Variables ---------------------------------------------------------
--------------------------------------------------------------------------------

collectFreeTyVars :: Parser a -> Parser ([Name], a)
collectFreeTyVars act = do
  ps <- getP
  putP $ ps { ps_freeTyVars = Just mempty }
  result <- act
  tvs <- map mkName . S.toList . fromJust . ps_freeTyVars <$> getP
  putP ps
  return (tvs, result)

visitTyVar :: String -> Parser ()
visitTyVar tv = modifyP $ \ps ->
  ps { ps_freeTyVars = S.insert tv <$> ps_freeTyVars ps }

--------------------------------------------------------------------------------
-- Identifier Parsers ----------------------------------------------------------
--------------------------------------------------------------------------------

fTyconP :: Parser FTycon
fTyconP = (FTcInt  <$ reserved "int")
      <|> (FTcInt  <$ reserved "Integer")
      <|> (FTcInt  <$ reserved "Int")
      <|> (FTcInt  <$ reserved "int")
      <|> (FTcReal <$ reserved "real")
      <|> (FTcBool <$ reserved "bool")
      <|> (FTcUser <$> conidP)

