{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Liquid.Parse (
    -- * Parser Type
    ParserT
  , runParserT
  , liftP

    -- * Parser State
  , getP
  , putP
  , modifyP

    -- * Position Information
  , position
  , withPos
  , located

    -- * Errors
  , formatError
  , parseFail
  , failAt
  , noParse
  , notExpecting

    -- * Parsing Utility Functions
  , named
  , unspacedIdent
  , optionBool

    -- * Language Definition
  , reserved, reservedOp, operator
  , whiteSpace, lexeme
  , natural
  , braces, brackets, parens
  , colon, comma

    -- * Identifier Parsers
  , qual
  , varidP, conidP, binderP
  , tyConP, tyConOpP
  , tyVarP, exprParamP
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Trans

import Data.Char
import Data.List

import qualified Data.HashSet as S

import Text.Parsec hiding ((<|>), runParserT)
import Text.Parsec.Error
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Pos
import Text.Parsec.Prim hiding ((<|>), runParserT)
import Text.Parsec.Token (GenLanguageDef(..))

import qualified Text.Parsec       as P
import qualified Text.Parsec.Token as T

import Language.Fixpoint.Types (Located(..))

--------------------------------------------------------------------------------
-- Parser Type -----------------------------------------------------------------
--------------------------------------------------------------------------------

type ParserT s m = ParsecT String (ParserState s) (ExceptT ParseError m)

data ParserState s
  = PS { ps_innerState :: s
       , ps_startPos   :: SourcePos
       }

runParserT :: Monad m
           => ParserT s m a
           -> s
           -> SourcePos
           -> String
           -> m (Either ParseError a)
runParserT p innerState startPos src = do
  result <- runExceptT act
  return $ case result of
    Left         err  -> Left err
    Right (Left  err) -> Left $ setErrorPos (shiftPos startPos $ errorPos err) err
    Right (Right out) -> Right out
  where
    act   = P.runParserT (whiteSpace *> p <* eof) state (sourceName startPos) src
    state = PS innerState startPos

liftP :: Monad m => m a -> ParserT s m a
liftP = lift . lift

--------------------------------------------------------------------------------
-- Parser State-----------------------------------------------------------------
--------------------------------------------------------------------------------

getP :: Monad m => ParserT s m s
getP = ps_innerState <$> getState

putP :: Monad m => s -> ParserT s m ()
putP s = do
  ps <- getState
  putState $ ps { ps_innerState = s }

modifyP :: Monad m => (s -> s) -> ParserT s m ()
modifyP f = modifyState $ \ps -> ps { ps_innerState = f $ ps_innerState ps }

--------------------------------------------------------------------------------
-- Position Information --------------------------------------------------------
--------------------------------------------------------------------------------

shiftPos :: SourcePos -> SourcePos -> SourcePos
shiftPos start pos = setSourceLine (setSourceColumn pos col') line'
  where
    line  = sourceLine   pos
    col   = sourceColumn pos

    line' = line + sourceLine start
    col'  | line == 1 = col + sourceColumn start
          | otherwise = col


position :: Monad m => ParserT s m SourcePos
position = do
  startPos <- ps_startPos <$> getState
  shiftPos startPos <$> getPosition

withPos :: Monad m => ParserT s m a -> ParserT s m (SourcePos, a)
withPos p = do
  s <- position
  a <- p
  return (s, a)

located :: Monad m => ParserT s m a -> ParserT s m (Located a)
located p = do
  s <- position
  a <- p
  e <- position -- TODO: End position minus whitespace!
  return $ Loc s e a

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------
--------------------------------------------------------------------------------

formatError :: String -> ParseError -> String
formatError src err = unlines $ x : line : caret : xs
  where
    line   = lines src !! (sourceLine pos - 1)
    caret  = replicate (sourceColumn pos - 1) ' '++ "^"
    (x:xs) = lines $ show err
    pos    = errorPos err


parseFail :: Monad m => String -> ParserT s m a
parseFail msg = do
  pos <- position
  failAt pos msg

failAt :: Monad m => SourcePos -> String -> ParserT s m a
failAt pos msg = throwError $ newErrorMessage (Message msg) pos

noParse :: Monad m => String -> ParserT s m a -> ParserT s m ()
noParse e p = do
  pos <- position
  (try p >> failAt pos e) <|> return ()

notExpecting :: Monad m => String -> ParserT s m a -> ParserT s m ()
notExpecting s p = noParse ("unexpected " ++ s) p

--------------------------------------------------------------------------------
-- Parsing Utility Functions ---------------------------------------------------
--------------------------------------------------------------------------------

named :: String -> ParserT s m a -> ParserT s m a
named s p = p <?> s

unspacedIdent :: Monad m =>  GenLanguageDef String (ParserState s) (ExceptT ParseError m) -> ParserT s m String
unspacedIdent def = try $ do
  name <- (:) <$> T.identStart def <*> many (T.identLetter def)
  if name `S.member` reservedSet
     then unexpected $ "reserved word " ++ show name
     else return name
  where
    reservedSet = S.fromList $ T.reservedNames def

optionBool :: Monad m => ParserT s m a -> ParserT s m Bool
optionBool p = option False (True <$ p)

--------------------------------------------------------------------------------
-- Language Definition ---------------------------------------------------------
--------------------------------------------------------------------------------

haskell :: Monad m => T.GenTokenParser String u m
haskell = T.makeTokenParser haskellDef

haskellDef :: Monad m => GenLanguageDef String u m
haskellDef = base
  { identLetter   = identLetter haskell98Def <|> char '#'
  , reservedNames = reservedNames base ++
      [ "foreign", "import", "export", "primitive"
      , "_ccall_", "_casm_"
      , "forall"
      ]
  }
  where
    base = haskell98Def

haskell98Def :: Monad m => GenLanguageDef String u m
haskell98Def = haskellStyle
  { reservedOpNames= ["::", "..", "=", "\\", "|", "<-", "->", "@", "~", "=>"]
  , reservedNames  = [ "let", "in", "case", "of", "if", "then", "else"
                     , "data", "type"
                     , "class", "default", "deriving", "do", "import"
                     , "infix", "infixl", "infixr", "instance", "module"
                     , "newtype", "where"
                     , "primitive"
                     ]
  }

haskellStyle :: Monad m => GenLanguageDef String u m
haskellStyle = emptyDef
  { commentStart    = "{-"
  , commentEnd      = "-}"
  , commentLine     = "--"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = opLetter haskellStyle
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames = []
  , reservedNames   = []
  , caseSensitive   = True
  }


reserved, reservedOp :: Monad m => String -> ParserT s m ()
reserved   = T.reserved haskell
reservedOp = T.reservedOp haskell

operator :: Monad m => ParserT s m String
operator = T.operator haskell


whiteSpace :: Monad m => ParserT s m ()
whiteSpace = T.whiteSpace haskell

lexeme :: Monad m => ParserT s m a -> ParserT s m a
lexeme = T.lexeme haskell

natural :: Monad m => ParserT s m Integer
natural = T.natural haskell

braces :: Monad m => ParserT s m a -> ParserT s m a
braces = T.braces haskell

brackets :: Monad m => ParserT s m a -> ParserT s m a
brackets = T.brackets haskell

parens :: Monad m => ParserT s m a -> ParserT s m a
parens = T.parens haskell

colon :: Monad m => ParserT s m String
colon = T.colon haskell

comma :: Monad m => ParserT s m String
comma = T.comma haskell

--------------------------------------------------------------------------------
-- Identifier Parsers ----------------------------------------------------------
--------------------------------------------------------------------------------

qual :: Monad m => ParserT s m String -> ParserT s m String
qual p = do
  q <- many (try (mod <* char '.'))
  x <- p
  return $ if null q
    then x
    else intercalate "." q ++ '.' : x
  where
    mod = named "module qualifier" $
      unspacedIdent $ haskellDef { T.identStart = upper }


varidP, conidP :: Monad m => ParserT s m String
varidP = named "variable identifier" $ T.identifier $ T.makeTokenParser $ haskellDef
  { T.identStart = lower <|> char '_' }
conidP = named "constructor identifier" $ T.identifier $ T.makeTokenParser $ haskellDef
  { T.identStart = upper }

binderP :: Monad m => ParserT s m String
binderP = named "binder" $ T.identifier $ T.makeTokenParser $ base
  { T.identStart = lower <|> char '_'
  , T.reservedNames = T.reservedNames base ++ ["true", "false", "not", "mod"]
  }
  where
    base = haskellDef


tyConP :: Monad m => ParserT s m String
tyConP = conidP <?> "type constructor"
 
tyConOpP :: Monad m => ParserT s m String
tyConOpP = named "type constructor operator" $ T.operator $ T.makeTokenParser $ haskellDef
  { T.identStart = satisfy (\c -> c /= ':' && isUpper c) <?> "uppercase letter" }


tyVarP :: Monad m => ParserT s m String
tyVarP = varidP <?> "type variable"

exprParamP :: Monad m => ParserT s m String
exprParamP = conidP <?> "expression parameter"

