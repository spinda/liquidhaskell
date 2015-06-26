module Language.Haskell.Liquid.HsExprToLogic (
    hsExprToLogic
  ) where

import GHC

import HsExpr
import Panic
import SrcLoc
import TysWiredIn

import Language.Fixpoint.Names
import Language.Fixpoint.Types

import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.WiredIn

--------------------------------------------------------------------------------

hsExprToLogic :: Var -> LHsExpr Id -> Ghc (Either Pred Expr)
hsExprToLogic var le
  | eqType rty boolTy = Left  <$> hsExprToPred le
  | otherwise         = Right <$> hsExprToExpr le
  where
    rty = snd $ splitFunTys $ snd $ splitForallTys $ varType var

--------------------------------------------------------------------------------

hsExprToExpr :: LHsExpr Id -> Ghc (Either Pred Expr)
hsExprToExpr le@(GenLocated s e) = go e
  where
    go (HsVar id) =
      return $ EVar $ symbol id
    go _ =
      unliftable $ showPpr le

--------------------------------------------------------------------------------

unliftable :: String -> Ghc a
unliftable = panic . ("Cannot lift to logic: " ++)

