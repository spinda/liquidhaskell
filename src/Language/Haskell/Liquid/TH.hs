module Language.Haskell.Liquid.TH (
    lq
  ) where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Language.Fixpoint.Types (val)

import           Language.Haskell.Liquid.Misc
import           Language.Haskell.Liquid.RefType (freeTyVars, quantifyRTy)

import           Language.Haskell.Liquid.TH.Magic
import           Language.Haskell.Liquid.TH.Misc
import           Language.Haskell.Liquid.TH.Parse
import           Language.Haskell.Liquid.TH.Simplify
import           Language.Haskell.Liquid.TH.Types
import           Language.Haskell.Liquid.TH.WiredIns

lq :: Bool -> QuasiQuoter
lq simplified =
  QuasiQuoter
    { quoteType = lqType simplified
    , quoteDec  = lqDec  simplified
    , quoteExp  = lqInvalid "expression"
    , quotePat  = lqInvalid "pattern"
    }


lqInvalid :: String -> String -> Q a
lqInvalid ctxt _ = fail $
  "`lq` quasiquoter cannot be used in the " ++ ctxt ++ " context"

lqDec :: Bool -> String -> Q [Dec]
lqDec = parseDecs

lqType :: Bool -> String -> Q Type
lqType simplified src = do
  (names, src') <- extractContext
  ty            <- parseType simplified src'
  newTVs        <- filterM (fmap isNothing . lookupTypeName) (freeTyVars $ val ty)
  ann           <- dataToExpQ' $ LiquidVar False False ty $ locatedSpan ty
  mapM_ (`forceAddAnnotation` ann) (map ValueAnnotation names)
  return $ simplifyRTy $ quantifyRTy newTVs $ val ty
  where
    extractContext
      | simplified =
        return ([], src)
      | lqTypeParsePrefix `isPrefixOf` src = do
        let (names, _:src') = break (== '|') $ drop (length lqTypeParsePrefix) src
        return (mkName <$> split ',' names, src')
      | otherwise = fail contextError
    contextError = unlines
      [ "The `lq` quasiquoter cannot generally be used in the type context."
      , "    Exceptions are type signatures for local variables, methods in class and"
      , "    instance declarations, constructors in GADTs, and fields in record "
      , "    constructors, to work around restrictions in the current version of GHC."
      , "    In these cases, the quasiquotation must appear directly after the `::` in"
      , "    the type signature."
      ]

