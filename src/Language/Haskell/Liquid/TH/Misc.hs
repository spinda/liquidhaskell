{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Liquid.TH.Misc (
    -- * Convert Data to Exp
    dataToExpQ'

    -- * Split AnnoType
  , splitSigmaRTy
  , splitForAllRTy
  , splitPhiRTy
  , splitCxtArrowRTy
  , splitCxtArrowRTy_maybe
  , splitTuplesRTy
  , splitTupleRTy
  , splitTupleRTy_maybe
  ) where

import Control.Applicative
import Control.Arrow

import Data.Data
import Data.List
import Data.Typeable

import qualified Data.Text as T

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Language.Haskell.Liquid.Types

import Language.Haskell.Liquid.TH.WiredIns

--------------------------------------------------------------------------------
-- Convert Data to Exp ---------------------------------------------------------
--------------------------------------------------------------------------------

dataToExpQ' :: Data a => a -> Q Exp
dataToExpQ' = dataToExpQ pack
  where
    pack :: Data b => b -> Maybe (Q Exp)
    pack x = (packStr <$> cast x) <|> (packText <$> cast x)

    packStr :: String -> Q Exp
    packStr = litE . StringL

    packText :: T.Text -> Q Exp
    packText = appE (varE 'T.pack) . packStr . T.unpack

--------------------------------------------------------------------------------
-- Split AnnoType --------------------------------------------------------------
--------------------------------------------------------------------------------

splitSigmaRTy :: AnnoType -> ([String], [AnnoType], AnnoType)
splitSigmaRTy ty =
  let (tvs,   rho) = splitForAllRTy ty
      (theta, tau) = splitPhiRTy rho
  in  (tvs, theta, tau)

splitForAllRTy :: AnnoType -> ([String], AnnoType)
splitForAllRTy ty = split ty []
  where
    split (RAllT tv ty) tvs = split ty (tv:tvs)
    split ty            tvs = (reverse tvs, ty)

splitPhiRTy :: AnnoType -> ([AnnoType], AnnoType)
splitPhiRTy ty = first concat $ split ty []
  where
    split ty cs = case splitCxtArrowRTy_maybe ty of
      Just (c, ty') -> split ty' (c:cs)
      Nothing       -> (reverse cs, ty)


splitCxtArrowRTy :: AnnoType -> ([AnnoType], AnnoType)
splitCxtArrowRTy ty = case splitCxtArrowRTy_maybe ty of
  Just out -> out
  Nothing  -> ([], ty)

splitCxtArrowRTy_maybe :: AnnoType -> Maybe ([AnnoType], AnnoType)
splitCxtArrowRTy_maybe (RApp c [cxt, ty] _ _)
  | val c == cxtArrowTcName = Just (splitTupleRTy cxt, ty)
splitCxtArrowRTy_maybe _ = Nothing


splitTuplesRTy :: AnnoType -> [AnnoType]
splitTuplesRTy t = case splitTupleRTy_maybe t of
  Just ts -> concatMap splitTuplesRTy ts
  Nothing -> [t]

splitTupleRTy :: AnnoType -> [AnnoType]
splitTupleRTy t = case splitTupleRTy_maybe t of
  Just ts -> ts
  Nothing -> [t]

splitTupleRTy_maybe :: AnnoType -> Maybe [AnnoType]
splitTupleRTy_maybe (RApp c as _ _) | val c == tupleTcName = Just as
splitTupleRTy_maybe _ = Nothing

