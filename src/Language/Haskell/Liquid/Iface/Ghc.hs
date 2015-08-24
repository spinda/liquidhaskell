module Language.Haskell.Liquid.Iface.Ghc (
    -- * Run IfL in TcM
    tcRnIfL

    -- * Interface File Wrapping
  , writeIfaceFile
  , readIfaceFile

    -- * Functions Extracted from GHC
  , bindIfaceTyVar
  , bindIfaceTyVars
  , tcIfaceTyCon
  ) where

import GHC

import BasicTypes
import Binary
import BinIface
import ConLike
import DataCon
import Exception
import FastMutInt
import FastString
import GhcMonad
import HscTypes
import Id
import IfaceEnv
import IfaceType
import Kind
import Module
import Name
import NameEnv
import Outputable
import PrelInfo
import TcEnv
import TcIface
import TcRnDriver
import TcRnMonad
import TcRnTypes
import TyCon
import Type
import TypeRep
import TysPrim
import UniqFM
import UniqSupply
import Unique
import Var

import Control.Monad

import Data.Array
import Data.Bits
import Data.Char
import Data.IORef
import Data.List
import Data.Word

--------------------------------------------------------------------------------
-- Run IfL in TcM --------------------------------------------------------------
--------------------------------------------------------------------------------

tcRnIfL :: Module -> IfL a -> TcM a
tcRnIfL mod act = do
  typeEnv <- getModTypeEnv mod
  initIfaceTc (emptyModIface mod) $ \ref ->
    liftIO (writeIORef ref typeEnv) >> act

getModTypeEnv :: Module -> TcM TypeEnv
getModTypeEnv mod = do
  top <- getTopEnv
  gbl <- getGblEnv
  if tcg_mod gbl == mod
     then liftIO $ readIORef $ tcg_type_env_var gbl
     else case lookupHptByModule (hsc_HPT top) mod of
            Just hmi -> return $ md_types $ hm_details hmi
            Nothing  -> eps_PTE <$> liftIO (readIORef $ hsc_EPS top)

--------------------------------------------------------------------------------
-- Interface File Wrapping -----------------------------------------------------
--------------------------------------------------------------------------------

-- | Adapted from GHC's writeBinIface
writeIfaceFile :: Binary a => FilePath -> a -> IO ()
writeIfaceFile path iface = do
  bh <- openBinMem initBinMemSize

  dictPtrPtr <- tellBin bh
  put_ bh dictPtrPtr

  symtabPtrPtr <- tellBin bh
  put_ bh symtabPtrPtr

  symtabNextRef <- newFastMutInt
  writeFastMutInt symtabNextRef 0
  symtabMapRef <- newIORef emptyUFM
  let binSymTab = BinSymbolTable symtabNextRef symtabMapRef

  dictNextRef <- newFastMutInt
  writeFastMutInt dictNextRef 0
  dictMapRef <- newIORef emptyUFM
  let binDict = BinDictionary dictNextRef dictMapRef

  bh <- return $ setUserData bh $ newWriteState (putName binDict binSymTab) (putFastString binDict)
  put_ bh iface

  symtabPtr <- tellBin bh
  putAt bh symtabPtrPtr symtabPtr
  seekBin bh symtabPtr

  symtabNext <- readFastMutInt symtabNextRef
  symtabMap <- readIORef symtabMapRef
  putSymbolTable bh symtabNext symtabMap

  dictPtr <- tellBin bh
  putAt bh dictPtrPtr dictPtr
  seekBin bh dictPtr

  dictNext <- readFastMutInt dictNextRef
  dictMap <- readIORef dictMapRef
  putDictionary bh dictNext dictMap

  writeBinMem bh path

-- | Adapted from GHC's readBinIface
readIfaceFile :: Binary a => FilePath -> TcM a
readIfaceFile path = do
  ncu <- mkNameCacheUpdater
  liftIO $ do
    bh <- readBinMem path

    dictPtr <- get bh
    dataPtr <- tellBin bh
    seekBin bh dictPtr
    dict <- getDictionary bh
    seekBin bh dataPtr
    bh <- return $ setUserData bh $ newReadState (error "getSymtabName") (getDictFastString dict)

    symtabPtr <- get bh
    dataPtr <- tellBin bh
    seekBin bh symtabPtr
    symtab <- getSymbolTable bh ncu
    seekBin bh dataPtr
    bh <- return $ setUserData bh $ newReadState (getSymtabName ncu dict symtab) (getDictFastString dict)

    get bh

--------------------------------------------------------------------------------
-- Types Extracted from GHC ----------------------------------------------------
--------------------------------------------------------------------------------

data BinSymbolTable = BinSymbolTable { bin_symtab_next :: !FastMutInt
                                     , bin_symtab_map  :: !(IORef (UniqFM (Int,Name)))
                                     }

data BinDictionary = BinDictionary { bin_dict_next :: !FastMutInt
                                   , bin_dict_map  :: !(IORef (UniqFM (Int,FastString)))
                                   }

type OnDiskName = (PackageKey, ModuleName, OccName)

--------------------------------------------------------------------------------
-- Functions Extracted from GHC ------------------------------------------------
--------------------------------------------------------------------------------

allocateFastString :: BinDictionary -> FastString -> IO Word32
allocateFastString BinDictionary { bin_dict_next = j_r,
                                   bin_dict_map  = out_r} f = do
  out <- readIORef out_r
  let uniq = getUnique f
  case lookupUFM out uniq of
    Just (j, _)  -> return (fromIntegral j :: Word32)
    Nothing -> do
     j <- readFastMutInt j_r
     writeFastMutInt j_r (j + 1)
     writeIORef out_r $! addToUFM out uniq (j, f)
     return (fromIntegral j :: Word32)

bindIfaceTyVar :: IfaceTvBndr -> (TyVar -> IfL a) -> IfL a
bindIfaceTyVar (occ,kind) thing_inside
  = do  { name <- newIfaceName (mkTyVarOccFS occ)
        ; tyvar <- mk_iface_tyvar name kind
        ; extendIfaceTyVarEnv [tyvar] (thing_inside tyvar) }

bindIfaceTyVars :: [IfaceTvBndr] -> ([TyVar] -> IfL a) -> IfL a
bindIfaceTyVars bndrs thing_inside
  = do { names <- newIfaceNames (map mkTyVarOccFS occs)
        ; let (kis_kind, tys_kind) = span isSuperIfaceKind kinds
              (kis_name, tys_name) = splitAt (length kis_kind) names
          -- We need to bring the kind variables in scope since type
          -- variables may mention them.
        ; kvs <- zipWithM mk_iface_tyvar kis_name kis_kind
        ; extendIfaceTyVarEnv kvs $ do
        { tvs <- zipWithM mk_iface_tyvar tys_name tys_kind
        ; extendIfaceTyVarEnv tvs (thing_inside (kvs ++ tvs)) } }
  where
    (occs,kinds) = unzip bndrs

fromOnDiskName :: Array Int Name -> NameCache -> OnDiskName -> (NameCache, Name)
fromOnDiskName _ nc (pid, mod_name, occ) =
    let mod   = mkModule pid mod_name
        cache = nsNames nc
    in case lookupOrigNameCache cache  mod occ of
           Just name -> (nc, name)
           Nothing   ->
               let (uniq, us) = takeUniqFromSupply (nsUniqs nc)
                   name       = mkExternalName uniq mod occ noSrcSpan
                   new_cache  = extendNameCache cache mod occ name
               in ( nc{ nsUniqs = us, nsNames = new_cache }, name )

getSymbolTable :: BinHandle -> NameCacheUpdater -> IO SymbolTable
getSymbolTable bh ncu = do
  sz <- get bh
  od_names <- sequence (replicate sz (get bh))
  updateNameCache ncu $ \namecache ->
    let arr = listArray (0,sz-1) names
        (namecache', names) =    
          mapAccumR (fromOnDiskName arr) namecache od_names
    in (namecache', arr)

initBinMemSize :: Int
initBinMemSize = 1024 * 1024

isSuperIfaceKind :: IfaceKind -> Bool
isSuperIfaceKind (IfaceTyConApp tc ITC_Nil) = ifaceTyConName tc == superKindTyConName
isSuperIfaceKind _ = False

knownKeyNamesMap :: UniqFM Name
knownKeyNamesMap = listToUFM_Directly [(nameUnique n, n) | n <- knownKeyNames]
  where
    knownKeyNames :: [Name]
    knownKeyNames = map getName wiredInThings ++ basicKnownKeyNames

mk_iface_tyvar :: Name -> IfaceKind -> IfL TyVar
mk_iface_tyvar name ifKind
   = do { kind <- tcIfaceKind ifKind
        ; return (Var.mkTyVar name kind) }

putName :: BinDictionary -> BinSymbolTable -> BinHandle -> Name -> IO ()
putName _dict BinSymbolTable{ 
               bin_symtab_map = symtab_map_ref,
               bin_symtab_next = symtab_next }    bh name
  | name `elemUFM` knownKeyNamesMap
  , let (c, u) = unpkUnique (nameUnique name)
  =
    put_ bh (0x40000000 .|. (fromIntegral (ord c) `shiftL` 22) .|. (fromIntegral u :: Word32))
  | otherwise
  = case wiredInNameTyThing_maybe name of
     Just (ATyCon tc)
       | isTupleTyCon tc             -> putTupleName_ bh tc 0
     Just (AConLike (RealDataCon dc))
       | let tc = dataConTyCon dc, isTupleTyCon tc -> putTupleName_ bh tc 1
     Just (AnId x)
       | Just dc <- isDataConWorkId_maybe x, let tc = dataConTyCon dc, isTupleTyCon tc -> putTupleName_ bh tc 2
     _ -> do
       symtab_map <- readIORef symtab_map_ref
       case lookupUFM symtab_map name of
         Just (off,_) -> put_ bh (fromIntegral off :: Word32)
         Nothing -> do
            off <- readFastMutInt symtab_next
            writeFastMutInt symtab_next (off+1)
            writeIORef symtab_map_ref
                $! addToUFM symtab_map name (off,name)
            put_ bh (fromIntegral off :: Word32)

putFastString :: BinDictionary -> BinHandle -> FastString -> IO ()
putFastString dict bh fs = allocateFastString dict fs >>= put_ bh

putTupleName_ :: BinHandle -> TyCon -> Word32 -> IO ()
putTupleName_ bh tc thing_tag
  = put_ bh (0x80000000 .|. (sort_tag `shiftL` 28) .|. (thing_tag `shiftL` 26) .|. arity)
  where
    arity = fromIntegral (tupleTyConArity tc)
    sort_tag = case tupleTyConSort tc of
      BoxedTuple      -> 0
      UnboxedTuple    -> 1
      ConstraintTuple -> 2

putSymbolTable :: BinHandle -> Int -> UniqFM (Int,Name) -> IO ()
putSymbolTable bh next_off symtab = do
  put_ bh next_off
  let names = elems (array (0,next_off-1) (eltsUFM symtab))
  mapM_ (\n -> serialiseName bh n symtab) names

serialiseName :: BinHandle -> Name -> UniqFM (Int,Name) -> IO ()
serialiseName bh name _ = do
    let mod = nameModule name
    put_ bh (modulePackageKey mod, moduleName mod, nameOccName name)

tcIfaceKind :: IfaceKind -> IfL Type
tcIfaceKind (IfaceAppTy t1 t2)  = do { t1' <- tcIfaceKind t1; t2' <- tcIfaceKind t2; return (AppTy t1' t2') }
tcIfaceKind (IfaceFunTy t1 t2)  = tcIfaceKindFun t1 t2
tcIfaceKind (IfaceDFunTy t1 t2) = tcIfaceKindFun t1 t2
tcIfaceKind (IfaceLitTy l)      = pprPanic "tcIfaceKind" (ppr l)
tcIfaceKind k                   = tcIfaceType k

tcIfaceKindFun :: IfaceKind -> IfaceKind -> IfL Type
tcIfaceKindFun t1 t2 = do { t1' <- tcIfaceKind t1; t2' <- tcIfaceKind t2; return (FunTy t1' t2') }

tcIfaceTcArgs :: IfaceTcArgs -> IfL [Type]
tcIfaceTcArgs args
  = case args of
      ITC_Type t ts ->
        do { t'  <- tcIfaceType t
           ; ts' <- tcIfaceTcArgs ts
           ; return (t':ts') }
      ITC_Kind k ks ->
        do { k'  <- tcIfaceKind k
           ; ks' <- tcIfaceTcArgs ks
           ; return (k':ks') }
      ITC_Nil -> return []

tcIfaceTyCon :: IfaceTyCon -> IfL TyCon
tcIfaceTyCon itc
  = do {
    ; thing <- tcIfaceGlobal (ifaceTyConName itc)
    ; case itc of
        IfaceTc _ -> return $ tyThingTyCon thing
        IfacePromotedDataCon _ -> return $ promoteDataCon $ tyThingDataCon thing
        IfacePromotedTyCon name ->
          let ktycon tc
                | isSuperKind (tyConKind tc) = return tc
                | Just prom_tc <- promotableTyCon_maybe tc = return prom_tc
                | otherwise = pprPanic "tcIfaceTyCon" (ppr name $$ ppr thing)
          in ktycon (tyThingTyCon thing)
    }

tcIfaceTyLit :: IfaceTyLit -> IfL TyLit
tcIfaceTyLit (IfaceNumTyLit n) = return (NumTyLit n)
tcIfaceTyLit (IfaceStrTyLit n) = return (StrTyLit n)

tcIfaceType :: IfaceType -> IfL Type
tcIfaceType (IfaceTyVar n)         = do { tv <- tcIfaceTyVar n; return (TyVarTy tv) }
tcIfaceType (IfaceAppTy t1 t2)     = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (AppTy t1' t2') }
tcIfaceType (IfaceLitTy l)         = do { l1 <- tcIfaceTyLit l; return (LitTy l1) }
tcIfaceType (IfaceFunTy t1 t2)     = tcIfaceTypeFun t1 t2
tcIfaceType (IfaceDFunTy t1 t2)    = tcIfaceTypeFun t1 t2
tcIfaceType (IfaceTyConApp tc tks) = do { tc' <- tcIfaceTyCon tc
                                        ; tks' <- tcIfaceTcArgs tks
                                        ; return (mkTyConApp tc' tks') }
tcIfaceType (IfaceForAllTy tv t)  = bindIfaceTyVar tv $ \ tv' -> do { t' <- tcIfaceType t; return (ForAllTy tv' t') }

tcIfaceTypeFun :: IfaceType -> IfaceType -> IfL Type
tcIfaceTypeFun t1 t2 = do { t1' <- tcIfaceType t1; t2' <- tcIfaceType t2; return (FunTy t1' t2') }

