{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Liquid.Literals (
        literalFRefType, literalFReft, literalConst, mkLit
        ) where

import TypeRep
import Literal 

import Language.Haskell.Liquid.Measure
import Language.Haskell.Liquid.Types
import Language.Haskell.Liquid.RefType

import qualified Language.Fixpoint.Types as F

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Monoid
import Control.Applicative

---------------------------------------------------------------
----------------------- Typing Literals -----------------------
---------------------------------------------------------------

makeRTypeBase (TyVarTy α)    x
  = RVar (rTyVar α) x
makeRTypeBase (TyConApp c ts) x
  = rApp c ((`makeRTypeBase` mempty) <$> ts) [] x
makeRTypeBase _              _
  = error "RefType : makeRTypeBase"

literalFRefType l
  = makeRTypeBase (literalType l) (literalFReft l)

literalFReft l = maybe mempty mkReft $ mkLit l

mkReft e = case e of
            F.ESym (F.SL str) ->
              -- FIXME: unsorted equality is shady, better to not embed Add# as int..
              F.meet (F.uexprReft e)
                     (F.reft "v" (F.PAtom F.Eq
                                  (F.EApp (name strLen) [F.EVar "v"])
                                  (F.ECon (F.I (fromIntegral (T.length str))))))
            _ -> F.exprReft e

-- | `literalConst` returns `Nothing` for unhandled lits because
--    otherwise string-literals show up as global int-constants
--    which blow up qualifier instantiation.

literalConst tce l         = (sort, mkLit l)
  where
    sort                   = typeSort tce $ literalType l


mkLit :: Literal -> Maybe F.Expr
mkLit (MachInt    n)   = mkI n
mkLit (MachInt64  n)   = mkI n
mkLit (MachWord   n)   = mkI n
mkLit (MachWord64 n)   = mkI n
mkLit (MachFloat  n)   = mkR n
mkLit (MachDouble n)   = mkR n
mkLit (LitInteger n _) = mkI n
mkLit (MachStr s)      = mkS s 
mkLit _                = Nothing -- ELit sym sort

mkI                    = Just . F.ECon . F.I  
mkR                    = Just . F.ECon . F.R  . fromRational
mkS                    = Just . F.ESym . F.SL . T.decodeUtf8

