module Language.Haskell.Liquid.Interface (
    buildInterface
  , packInterface

  , encodeInterface
  , decodeInterface

  , loadInterface
  , saveInterface
  ) where

--------------------------------------------------------------------------------

import GHC
import NameSet

import Control.Applicative
import Control.Arrow
import System.Directory
import System.FilePath
import Text.PrettyPrint.HughesPJ (text)

import qualified Control.Exception   as Ex
import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

import Language.Fixpoint.Types (Symbol, symbol)

import Language.Haskell.Liquid.Errors ()
import Language.Haskell.Liquid.GhcMisc (qualifiedNameSymbol)
import Language.Haskell.Liquid.Misc (first3)
import Language.Haskell.Liquid.Serialize
import Language.Haskell.Liquid.Tidy
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.Measure as Ms

--------------------------------------------------------------------------------

buildInterface :: Integer -> Ms.BareSpec -> GhcSpec -> [ModuleName] -> RInterface
buildInterface checksum bareSpec ghcSpec imports
  = Intr { intr_checksum = checksum
         , intr_imports  = imports
         , intr_includes = Ms.includes bareSpec
         , intr_fnSigs   = M.fromList exportedFnSigs
         , intr_meaSigs  = M.fromList measureSigs
         }
  where
    -- TODO: Do we *need* to convert these all to Symbols?
    exportedSymbols
      = S.fromList $ map qualifiedNameSymbol $ nameSetToList $ exports ghcSpec

    exportedFnSigs
      = filter ((`S.member` exportedSymbols) . fst) allFnSigs
    allFnSigs
      = map (symbol *** val) (tySigs ghcSpec)

    measureSigs
      = map (second val) (meas ghcSpec)

--------------------------------------------------------------------------------

packInterface :: RInterface -> BInterface
packInterface intr
  = Intr { intr_checksum = intr_checksum intr
         , intr_imports  = intr_imports intr
         , intr_includes = intr_includes intr
         , intr_fnSigs   = M.map packSpecType (intr_fnSigs  intr)
         , intr_meaSigs  = M.map packSpecType (intr_meaSigs intr)
         }


packSpecType :: SpecType -> BareType
packSpecType
  = packRRType . tidySpecType Full

packRRType :: RRType r -> BRType r
packRRType (RVar tv r)
  = RVar (symbol tv) r
packRRType (RFun b i o r)
  = RFun b (packRRType i) (packRRType o) r
packRRType (RAllT tv ty)
  = RAllT (symbol tv) (packRRType ty)
packRRType (RAllP pv ty)
  = RAllP (packRPVar pv) (packRRType ty)
packRRType (RAllS b ty)
  = RAllS b (packRRType ty)
packRRType (RApp tyc args pargs r)
  = RApp (dummyLoc $ symbol $ rtc_tc tyc) (map packRRType args) (map packRTProp pargs) r
packRRType (RAllE b arg ty)
  = RAllE b (packRRType arg) (packRRType ty)
packRRType (REx b arg ty)
  = REx b (packRRType arg) (packRRType ty)
packRRType (RExprArg expr)
  = RExprArg expr
packRRType (RAppTy arg res r)
  = RAppTy (packRRType arg) (packRRType res) r
packRRType (RRTy env r obl ty)
  = RRTy (map (second packRRType) env) r obl (packRRType ty)
packRRType (ROth s)
  = ROth s
packRRType (RHole r)
  = RHole r

packRPVar :: RPVar -> BPVar
packRPVar pvar
  = pvar { ptype = packPVKind (ptype pvar)
         , pargs = map (first3 packRRType) (pargs pvar)
         }

packPVKind :: PVKind RSort -> PVKind BSort
packPVKind (PVProp ty)
  = PVProp (packRRType ty)
packPVKind PVHProp
  = PVHProp

packRTProp :: RTProp RTyCon RTyVar r -> RTProp LocSymbol Symbol r
packRTProp (RPropP args r)
  = RPropP (map (second packRRType) args) r
packRTProp (RProp args ty)
  = RProp (map (second packRRType) args) (packRRType ty)
packRTProp (RHProp args w)
  = RHProp (map (second packRRType) args) (packWorld w)

packWorld :: World (RRType r) -> World (BRType r)
packWorld (World hs)
  = World (map packHSeg hs)

packHSeg :: HSeg (RRType r) -> HSeg (BRType r)
packHSeg (HBind addr ty)
  = HBind addr (packRRType ty)
packHSeg (HVar pv)
  = HVar pv

--------------------------------------------------------------------------------

encodeInterface :: BInterface -> B.ByteString
encodeInterface
  = encode
{-# INLINE encodeInterface #-}

decodeInterface :: B.ByteString -> Either String BInterface
decodeInterface
  = decode
{-# INLINE decodeInterface #-}

--------------------------------------------------------------------------------

lqhiFilePath :: FilePath -> FilePath
lqhiFilePath = (`replaceExtension` "lqhi")


loadInterface :: FilePath -> IO (Maybe BInterface)
loadInterface specFile
  = do exists <- doesFileExist lqhiFile
       if exists
          then Just <$> loadInterface' lqhiFile
          else return Nothing
  where
    lqhiFile = lqhiFilePath specFile

loadInterface' :: FilePath -> IO BInterface
loadInterface' lqhiFile
  = do bytes <- B.readFile lqhiFile
       -- TODO: Improve error handling here
       either (Ex.throw . errOther . text) return (decodeInterface bytes)


saveInterface :: FilePath -> BInterface -> IO ()
saveInterface specFile intr
  = B.writeFile lqhiFile (encodeInterface intr)
  where
    lqhiFile = lqhiFilePath specFile

