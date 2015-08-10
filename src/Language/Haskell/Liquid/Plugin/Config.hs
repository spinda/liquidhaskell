module Language.Haskell.Liquid.Plugin.Config (
    -- * Parse Config from Plugin Options
    parsePluginOpts
  ) where

import GHC

import GhcPlugins hiding (char, comma, brackets)
import Panic

import Control.Monad
import Control.Monad.Identity

import Data.List
import Data.Maybe

import System.Console.CmdArgs.Verbosity
import System.Directory

import Text.Parsec hiding (runParserT)
import Text.Parsec.Combinator
import Text.Parsec.Pos

import Language.Fixpoint.Config (SMTSolver(..))
import Language.Fixpoint.Misc hiding (Loud)

import Language.Haskell.Liquid.Parse
import Language.Haskell.Liquid.Types

--------------------------------------------------------------------------------
-- Parse Config from Plugin Options --------------------------------------------
--------------------------------------------------------------------------------

parsePluginOpts :: [CommandLineOption] -> IO Config
parsePluginOpts opts =
  case runIdentity $ runParserT act () start src of
    Left err ->
      throwGhcExceptionIO $ CmdLineError $ '\n' : formatError src err ++ usage
    Right cfg ->
      resolveConfig cfg
  where
    act   = configP defaultConfig
    start = newPos "plugin options" 0 0
    src   = filter (/= '\n') $ intercalate "," $ reverse opts
    usage = "\n\nFor usage information, see: https://github.com/ucsd-progsys/liquidhaskell"

--------------------------------------------------------------------------------
-- Internal Parser -------------------------------------------------------------
--------------------------------------------------------------------------------

type Parser = ParserT () Identity

configP :: Config -> Parser Config
configP cfg = do
  cfg' <- optionP cfg
  option cfg' $ comma >> configP cfg'

optionP :: Config -> Parser Config
optionP cfg = do
  pos <- position
  key <- flag

  let noArg result =
        result <$ noParse ("flag '" ++ key ++ "' takes no argument") (reservedOp "=")
      arg parser fn = do
        reservedOp "=" <|> parseFail ("expected '=' and argument to flag '" ++ key ++ "'")
        fn <$> parser

  case key of
    ""                  -> return cfg

    "verbose"           -> noArg $ cfg { verbose        = True }
    "real"              -> noArg $ cfg { real           = True }
    "native"            -> noArg $ cfg { native         = True }
    "no-verify"         -> noArg $ cfg { noVerify       = True }
    "no-write-iface"    -> noArg $ cfg { noWriteIface   = True }
    "no-check-unknown"  -> noArg $ cfg { noCheckUnknown = True }
    "no-termination"    -> noArg $ cfg { notermination  = True }
    "no-warnings"       -> noArg $ cfg { nowarnings     = True }
    "trust-internals"   -> noArg $ cfg { trustinternals = True }
    "no-case-expand"    -> noArg $ cfg { nocaseexpand   = True }
    "strata"            -> noArg $ cfg { strata         = True }
    "notruetypes"       -> noArg $ cfg { notruetypes    = True }
    "totality"          -> noArg $ cfg { totality       = True }
    "no-prune"          -> noArg $ cfg { noPrune        = True }
    "short-names"       -> noArg $ cfg { shortNames     = True }
    "short-errors"      -> noArg $ cfg { shortErrors    = True }
    "no-ghc-prim-specs" -> noArg $ cfg { noGhcPrimSpecs = True }
    "no-base-specs"     -> noArg $ cfg { noBaseSpecs    = True }

    "diffcheck"         -> noArg $
      cfg { diffcheck = True
          , fullcheck = False
          }
    "fullcheck"         -> noArg $
      cfg { diffcheck = False
          , fullcheck = True
          }

    "binders"           ->
      arg (brackets $ sepBy varidP comma) $ \bndrs ->
        cfg { binders = bndrs ++ binders cfg }
    "max-params"        ->
      arg natural' $ \max ->
        cfg { maxParams = max }
    "smt-solver"        ->
      arg smtSolver $ \smt ->
        cfg { smtsolver = Just smt }

    _                   -> failAt pos $ "unrecognized flag '" ++ key ++ "'"

--------------------------------------------------------------------------------
-- Check/Resolve Config --------------------------------------------------------
--------------------------------------------------------------------------------

resolveConfig :: Config -> IO Config
resolveConfig cfg = do
  when (verbose cfg) $ setVerbosity Loud
  case smtsolver cfg of
    Just _  -> return cfg'
    Nothing -> do
      smts <- mapM findSmtSolver [Z3, Cvc4, Mathsat]
      case catMaybes smts of
        (s:_) -> return (cfg' { smtsolver = Just s })
        _     -> throwGhcExceptionIO $ CmdLineError noSmtError
  where
    cfg'       = cfg { binders = sortNub $ filter (/= "") $ binders cfg }
    noSmtError = "LiquidHaskell requires an SMT Solver, i.e. z3, cvc4, or mathsat to be installed."

findSmtSolver :: SMTSolver -> IO (Maybe SMTSolver)
findSmtSolver smt =
  maybe Nothing (const $ Just smt) <$> findExecutable (show smt)

--------------------------------------------------------------------------------
-- Parsing Utility Functions ---------------------------------------------------
--------------------------------------------------------------------------------

flag :: Parser String
flag = lexeme (many (alphaNum <|> char '-') <?> "flag")

natural' :: Parser Int
natural' = do
  p <- position
  n <- natural
  if n > fromIntegral (maxBound :: Int)
     then failAt p $ "number is too big (max is " ++ show (maxBound :: Int) ++ ")"
     else return $ fromIntegral n

smtSolver :: Parser SMTSolver
smtSolver = (Z3      <$ reserved "z3")
        <|> (Cvc4    <$ reserved "cvc4")
        <|> (Mathsat <$ reserved "mathsat")
        <|> (Z3mem   <$ reserved "z3mem")

