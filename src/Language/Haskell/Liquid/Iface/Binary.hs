{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Haskell.Liquid.Iface.Binary () where

import Binary
import IfaceType (IfLclName)

import qualified Data.Text as T

import Text.Parsec.Pos

import Language.Fixpoint.Types hiding (Predicate)

import Language.Haskell.Liquid.Types hiding (L, R)
import Language.Haskell.Liquid.Variance

import Language.Haskell.Liquid.Iface.Types

--------------------------------------------------------------------------------
-- Binary Instances for Liquid Types -------------------------------------------
--------------------------------------------------------------------------------

instance Binary spec => Binary (IfaceData spec) where
  put_ bh (ID {..}) = do
    put_ bh ifaceModule
    put_ bh ifaceFingerprint
    put_ bh ifaceSpec
  get bh = ID <$> get bh <*> get bh <*> get bh

instance Binary IfaceSpec where
  put_ bh (IS {..}) = do
    put_ bh ifaceTySigs
    put_ bh ifaceAsmSigs
    put_ bh ifaceCtors
    put_ bh ifaceMeas
    put_ bh ifaceInvariants
    put_ bh ifaceIAliases
    put_ bh ifaceFreeSyms
    put_ bh ifaceTcEmbeds
    put_ bh ifaceQualifiers
    put_ bh ifaceTyConEnv
    put_ bh ifaceRTEnv
    put_ bh ifaceTInlines
    put_ bh ifaceExports
  get bh = do
    IS <$> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh
       <*> get bh

instance Binary r => Binary (IRType r) where
  put_ bh (RVar tv r)       = putByte bh  0 >> put_ bh tv  >> put_ bh r
  put_ bh (RFun b i o r)    = putByte bh  1 >> put_ bh b   >> put_ bh i  >> put_ bh o >> put_ bh r
  put_ bh (RAllT tv ty)     = putByte bh  2 >> put_ bh tv  >> put_ bh ty
  put_ bh (RAllP pv ty)     = putByte bh  3 >> put_ bh pv  >> put_ bh ty
  put_ bh (RAllS sb ty)     = putByte bh  4 >> put_ bh sb  >> put_ bh ty
  put_ bh (RApp tc as ps r) = putByte bh  5 >> put_ bh tc  >> put_ bh as >> put_ bh ps >> put_ bh r
  put_ bh (RAllE b a ty)    = putByte bh  6 >> put_ bh b   >> put_ bh a  >> put_ bh ty
  put_ bh (REx b e ty)      = putByte bh  7 >> put_ bh b   >> put_ bh e  >> put_ bh ty
  put_ bh (RExprArg e)      = putByte bh  8 >> put_ bh e
  put_ bh (RAppTy t1 t2 r)  = putByte bh  9 >> put_ bh t1  >> put_ bh t2 >> put_ bh r
  put_ bh (RRTy env r o ty) = putByte bh 10 >> put_ bh env >> put_ bh r  >> put_ bh o >> put_ bh ty
  put_ bh (RHole r)         = putByte bh 11 >> put_ bh r

  get bh = do
    h <- getByte bh
    case h of
      0  -> RVar     <$> get bh <*> get bh
      1  -> RFun     <$> get bh <*> get bh <*> get bh <*> get bh
      2  -> RAllT    <$> get bh <*> get bh
      3  -> RAllP    <$> get bh <*> get bh
      4  -> RAllS    <$> get bh <*> get bh
      5  -> RApp     <$> get bh <*> get bh <*> get bh <*> get bh
      6  -> RAllE    <$> get bh <*> get bh <*> get bh
      7  -> REx      <$> get bh <*> get bh <*> get bh
      8  -> RExprArg <$> get bh
      9  -> RAppTy   <$> get bh <*> get bh <*> get bh
      10 -> RRTy     <$> get bh <*> get bh <*> get bh <*> get bh
      11 -> RHole    <$> get bh
      _  -> error $ "Invalid RType id " ++ show h

instance (Binary τ, Binary r, Binary t) => Binary (Ref τ r t) where
  put_ bh (RPropP as r) = putByte bh 0 >> put_ bh as >> put_ bh r
  put_ bh (RProp as b)  = putByte bh 1 >> put_ bh as >> put_ bh b
  put_ bh (RHProp as h) = putByte bh 2 >> put_ bh as >> put_ bh h

  get bh = do
    h <- getByte bh
    case h of
      0 -> RPropP <$> get bh <*> get bh
      1 -> RProp  <$> get bh <*> get bh
      2 -> RHProp <$> get bh <*> get bh
      _ -> error $ "Invalid Ref id " ++ show h

instance Binary t => Binary (World t) where
  put_ bh (World hs) = put_ bh hs
  get  bh            = World <$> get bh

instance Binary t => Binary (HSeg t) where
  put_ bh (HBind a v) = putByte bh 0 >> put_ bh a >> put_ bh v
  put_ bh (HVar pv)   = putByte bh 1 >> put_ bh pv

  get bh = do
    h <- getByte bh
    case h of
      0 -> HBind <$> get bh <*> get bh
      1 -> HVar  <$> get bh
      _ -> error $ "Invalid HSeg id " ++ show h

instance Binary a => Binary (Located a) where
  put_ bh (Loc start end val) = do
    put_ bh start
    put_ bh end
    put_ bh val
  get bh = Loc <$> get bh <*> get bh <*> get bh

instance Binary SourcePos where
  put_ bh pos = do
    put_ bh $ sourceName   pos
    put_ bh $ sourceLine   pos
    put_ bh $ sourceColumn pos
  get bh = newPos <$> get bh <*> get bh <*> get bh 

instance Binary Symbol where
  put_ bh = put_ bh . symbolString
  get  bh = symbol <$> (get bh :: IO String)

instance Binary r => Binary (UReft r) where
  put_ bh (U r p s) = do
    put_ bh r
    put_ bh p
    put_ bh s
  get bh = U <$> get bh <*> get bh <*> get bh

instance Binary Predicate where
  put_ bh (Pr pvs) = put_ bh pvs
  get  bh          = Pr <$> get bh

instance Binary Stratum where
  put_ bh (SVar v) = putByte bh 0 >> put_ bh v
  put_ bh SDiv     = putByte bh 1
  put_ bh SWhnf    = putByte bh 2
  put_ bh SFin     = putByte bh 3

  get bh = do
    h <- getByte bh
    case h of
      0 -> SVar <$> get bh
      1 -> return SDiv
      2 -> return SWhnf
      3 -> return SFin
      _ -> error $ "Invalid Stratum id " ++ show h

instance Binary t => Binary (PVar t) where
  put_ bh (PV {..}) = do
    put_ bh pname
    put_ bh ptype
    put_ bh parg
    put_ bh pargs
  get bh = PV <$> get bh <*> get bh <*> get bh <*> get bh

instance Binary t => Binary (PVKind t) where
  put_ bh (PVProp t) = putByte bh 0 >> put_ bh t
  put_ bh PVHProp    = putByte bh 1

  get bh = do
    h <- getByte bh
    case h of
      0 -> PVProp <$> get bh
      1 -> return PVHProp
      _ -> error $ "Invalid PVKind id " ++ show h

instance Binary Reft where
  put_ bh (Reft r) = put_ bh r
  get  bh          = Reft <$> get bh

instance Binary Refa where
  put_ bh (Refa p) = put_ bh p
  get  bh          = Refa <$> get bh

instance Binary Pred where
  put_ bh PTrue           = putByte bh  0
  put_ bh PFalse          = putByte bh  1
  put_ bh (PAnd ps)       = putByte bh  2 >> put_ bh ps
  put_ bh (POr ps)        = putByte bh  3 >> put_ bh ps
  put_ bh (PNot p)        = putByte bh  4 >> put_ bh p
  put_ bh (PImp p1 p2)    = putByte bh  5 >> put_ bh p1 >> put_ bh p2
  put_ bh (PIff p1 p2)    = putByte bh  6 >> put_ bh p1 >> put_ bh p2
  put_ bh (PBexp e)       = putByte bh  7 >> put_ bh e
  put_ bh (PAtom b e1 e2) = putByte bh  8 >> put_ bh b  >> put_ bh e1 >> put_ bh e2
  put_ bh (PKVar kv sb)   = putByte bh  9 >> put_ bh kv >> put_ bh sb
  put_ bh (PAll ss p)     = putByte bh 10 >> put_ bh ss >> put_ bh p
  put_ bh PTop            = putByte bh 11

  get bh = do
    h <- getByte bh
    case h of
      0  -> return PTrue
      1  -> return PFalse
      2  -> PAnd  <$> get bh
      3  -> POr   <$> get bh
      4  -> PNot  <$> get bh
      5  -> PImp  <$> get bh <*> get bh
      6  -> PIff  <$> get bh <*> get bh
      7  -> PBexp <$> get bh
      8  -> PAtom <$> get bh <*> get bh <*> get bh
      9  -> PKVar <$> get bh <*> get bh
      10 -> PAll  <$> get bh <*> get bh
      11 -> return PTop
      _  -> error $ "Invalid Pred id " ++ show h

instance Binary Brel where
  put_ bh Eq  = putByte bh 0
  put_ bh Ne  = putByte bh 1
  put_ bh Gt  = putByte bh 2
  put_ bh Ge  = putByte bh 3
  put_ bh Lt  = putByte bh 4
  put_ bh Le  = putByte bh 5
  put_ bh Ueq = putByte bh 6
  put_ bh Une = putByte bh 7

  get bh = do
    h <- getByte bh
    return $ case h of
      0 -> Eq
      1 -> Ne
      2 -> Gt
      3 -> Ge
      4 -> Lt
      5 -> Le
      6 -> Ueq
      7 -> Une
      _ -> error $ "Invalid Brel id " ++ show h

instance Binary KVar where
  put_ bh (KV s) = put_ bh s
  get  bh        = KV <$> get bh

instance Binary Subst where
  put_ bh (Su ss) = put_ bh ss
  get  bh         = Su <$> get bh

instance Binary Expr where
  put_ bh (ESym sc)      = putByte bh 0 >> put_ bh sc
  put_ bh (ECon c)       = putByte bh 1 >> put_ bh c
  put_ bh (EVar v)       = putByte bh 2 >> put_ bh v
  put_ bh (ELit ls ss)   = putByte bh 3 >> put_ bh ls >> put_ bh ss
  put_ bh (EApp ls es)   = putByte bh 4 >> put_ bh ls >> put_ bh es
  put_ bh (ENeg e)       = putByte bh 5 >> put_ bh e
  put_ bh (EBin b e1 e2) = putByte bh 6 >> put_ bh b  >> put_ bh e1 >> put_ bh e2
  put_ bh (EIte p e1 e2) = putByte bh 7 >> put_ bh p  >> put_ bh e1 >> put_ bh e2
  put_ bh (ECst e s)     = putByte bh 8 >> put_ bh e  >> put_ bh s
  put_ bh EBot           = putByte bh 9

  get bh = do
    h <- getByte bh
    case h of
      0 -> ESym <$> get bh
      1 -> ECon <$> get bh
      2 -> EVar <$> get bh
      3 -> ELit <$> get bh <*> get bh
      4 -> EApp <$> get bh <*> get bh
      5 -> ENeg <$> get bh
      6 -> EBin <$> get bh <*> get bh <*> get bh
      7 -> EIte <$> get bh <*> get bh <*> get bh
      8 -> ECst <$> get bh <*> get bh
      9 -> return EBot
      _ -> error $ "Invalid Expr id " ++ show h

instance Binary Bop where
  put_ bh Plus  = putByte bh 0
  put_ bh Minus = putByte bh 1
  put_ bh Times = putByte bh 2
  put_ bh Div   = putByte bh 3
  put_ bh Mod   = putByte bh 4

  get bh = do
    h <- getByte bh
    return $ case h of
      0 -> Plus
      1 -> Minus
      2 -> Times
      3 -> Div
      4 -> Mod
      _ -> error $ "Invalid Bop id " ++ show h

instance Binary SymConst where
  put_ bh (SL txt) = put_ bh txt
  get  bh          = SL <$> get bh

instance Binary T.Text where
  put_ bh = put_ bh . T.unpack
  get  bh = T.pack <$> get bh

instance Binary Constant where
  put_ bh (I val  ) = putByte bh 0 >> put_ bh val
  put_ bh (R val  ) = putByte bh 1 >> put_ bh val
  put_ bh (L val s) = putByte bh 2 >> put_ bh val >> put_ bh s

  get bh = do
    h <- getByte bh
    case h of
      0 -> I <$> get bh
      1 -> R <$> get bh
      2 -> L <$> get bh <*> get bh
      _ -> error $ "Invalid Constant id " ++ show h

-- TODO: Replace this instance before actual use
instance Binary Double where
  put_ bh = put_ bh . show
  get  bh = read <$> get bh

instance Binary Sort where
  put_ bh FInt          = putByte bh 0
  put_ bh FReal         = putByte bh 1
  put_ bh FNum          = putByte bh 2
  put_ bh FFrac         = putByte bh 3
  put_ bh (FObj s)      = putByte bh 4 >> put_ bh s
  put_ bh (FVar i)      = putByte bh 5 >> put_ bh i
  put_ bh (FFunc i ss)  = putByte bh 6 >> put_ bh i   >> put_ bh ss
  put_ bh (FApp ftc ss) = putByte bh 7 >> put_ bh ftc >> put_ bh ss

  get bh = do
    h <- getByte bh
    case h of
      0 -> return FInt
      1 -> return FReal
      2 -> return FNum
      3 -> return FFrac
      4 -> FObj  <$> get bh
      5 -> FVar  <$> get bh
      6 -> FFunc <$> get bh <*> get bh
      7 -> FApp  <$> get bh <*> get bh
      _ -> error $ "Invalid Sort id " ++ show h

instance Binary FTycon where
  put_ bh = put_ bh . fTyconSymbol
  get  bh = symbolFTycon <$> get bh

instance Binary Oblig where
  put_ bh OTerm = putByte bh 0
  put_ bh OInv  = putByte bh 1
  put_ bh OCons = putByte bh 2

  get bh = do
    h <- getByte bh
    return $ case h of
      0 -> OTerm
      1 -> OInv
      2 -> OCons
      _ -> error $ "Invalid Oblig id " ++ show h

instance Binary IfaceMeasure where
  put_ bh (M name sort defs) = do
    put_ bh name
    put_ bh sort
    put_ bh defs
  get bh = M <$> get bh <*> get bh <*> get bh

instance Binary ITyCon where
  put_ bh (ITyCon tc pv info) = do
    put_ bh tc
    put_ bh pv
    put_ bh info
  get bh = ITyCon <$> get bh <*> get bh <*> get bh

instance Binary TyConInfo where
  put_ bh (TyConInfo tys pss sfn) = do
    put_ bh tys
    put_ bh pss
    put_ bh sfn
  get bh = TyConInfo <$> get bh <*> get bh <*> get bh

instance Binary Variance where
  put_ bh Invariant     = putByte bh 0
  put_ bh Bivariant     = putByte bh 1
  put_ bh Contravariant = putByte bh 2
  put_ bh Covariant     = putByte bh 3

  get bh = do
    h <- getByte bh
    return $ case h of
      0 -> Invariant
      1 -> Bivariant
      2 -> Contravariant
      3 -> Covariant
      _ -> error $ "Invalid Variance id " ++ show h

instance Binary SizeFunction where
  put_ bh NumSizeFun = putByte bh 0
  put_ bh LenSizeFun = putByte bh 1

  get bh = do
    h <- getByte bh
    return $ case h of
      0 -> NumSizeFun
      1 -> LenSizeFun
      _ -> error $ "Invalid SizeFunction id " ++ show h

instance Binary Qualifier where
  put_ bh (Q {..}) = do
    put_ bh q_name
    put_ bh q_params
    put_ bh q_body
    put_ bh q_pos
  get bh = Q <$> get bh <*> get bh <*> get bh <*> get bh

instance Binary (RTAlias IfLclName IfaceType) where
  put_ bh (RTA {..}) = do
    put_ bh rtTArgs
    put_ bh rtEArgs
    put_ bh rtBody
  get bh = RTA <$> get bh <*> get bh <*> get bh

instance Binary TInline where
  put_ bh (TI {..}) = do
    put_ bh tiargs
    put_ bh tibody
  get bh = TI <$> get bh <*> get bh

