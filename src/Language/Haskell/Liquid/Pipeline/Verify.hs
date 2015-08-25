{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.Liquid.Pipeline.Verify (
    -- * LiquidHaskell Verification Pipeline
    liquidOne
  ) where

import GHC

import CoreSyn
import Var

import Control.Arrow (second)
import Control.Monad
import Control.DeepSeq

import Data.Either
import Data.List  hiding (intersperse)
import Data.Maybe

import qualified Data.HashMap.Strict as M

import System.Console.CmdArgs.Verbosity (whenLoud)
import System.Console.CmdArgs.Default

import System.FilePath

import Text.PrettyPrint.HughesPJ

import Language.Fixpoint.Files hiding (Result)
import Language.Fixpoint.Interface
import Language.Fixpoint.Misc
import Language.Fixpoint.Names
import Language.Fixpoint.Types

import qualified Language.Fixpoint.Config as FC
import qualified Language.Fixpoint.Files  as FF

import Language.Haskell.Liquid.Constraint
import Language.Haskell.Liquid.Errors
import Language.Haskell.Liquid.GhcMisc
import Language.Haskell.Liquid.Misc
import Language.Haskell.Liquid.PrettyPrint
import Language.Haskell.Liquid.RefType
import Language.Haskell.Liquid.Types

import qualified Language.Haskell.Liquid.DiffCheck as DC

import Language.Haskell.Liquid.Pipeline.Annotate (annotate, mkOutput)
import Language.Haskell.Liquid.Pipeline.TransformRec

--------------------------------------------------------------------------------
-- LiquidHaskell Verification Pipeline -----------------------------------------
--------------------------------------------------------------------------------

liquidOne :: FilePath -> GhcInfo -> IO (Output Doc)
liquidOne target info =
  do donePhase Loud "Extracted Core using GHC"
     let cfg   = config info
     whenLoud  $ do putStrLn "**** Config **************************************************"
                    print cfg
     whenLoud  $ do putStrLn $ showpp info
                    putStrLn "*************** Original CoreBinds ***************************"
                    putStrLn $ showpp (cbs info)
     let cbs' = transformScope (cbs info)
     whenLoud  $ do donePhase Loud "transformRecExpr"
                    putStrLn "*************** Transform Rec Expr CoreBinds *****************"
                    putStrLn $ showpp cbs'
                    putStrLn "*************** Slicing Out Unchanged CoreBinds *****************"
     dc <- prune cfg cbs' target info
     let cbs'' = maybe cbs' DC.newBinds dc
     let info' = maybe info (\z -> info {spec = DC.newSpec z}) dc
     let cgi   = {-# SCC "generateConstraints" #-} generateConstraints $! info' {cbs = cbs''}
     cgi `deepseq` donePhase Loud "generateConstraints"
     out      <- solveCs cfg target cgi info' dc
     donePhase Loud "solve"
     let out'  = mconcat [maybe mempty DC.oldOutput dc, out]
     DC.saveResult target out'
     exitWithResult cfg target out'

checkedNames ::  Maybe DC.DiffCheck -> Maybe [String]
checkedNames dc          = concatMap names . DC.newBinds <$> dc
   where
     names (NonRec v _ ) = [showpp $ shvar v]
     names (Rec xs)      = map (shvar . fst) xs
     shvar               = showpp . varName

prune :: Config -> [CoreBind] -> FilePath -> GhcInfo -> IO (Maybe DC.DiffCheck)
prune cfg cbinds target info
  | not (null vs) = return . Just $ DC.DC (DC.thin cbinds vs) mempty sp
  | diffcheck cfg = DC.slice target cbinds sp
  | otherwise     = return Nothing
  where
    vs            = tgtVars sp
    sp            = spec info

solveCs :: Config -> FilePath -> CGInfo -> GhcInfo -> Maybe DC.DiffCheck -> IO (Output Doc)
solveCs cfg target cgi info dc
  = do let finfo = targetFInfo info cgi
       Result r sol <- solve fx finfo
       let names = checkedNames dc
       let warns = logErrors cgi
       let annm  = annotMap cgi
       let res   = ferr sol r
       let out0  = mkOutput cfg res sol annm
       return    $ out0 { o_vars = names } { o_errors  = warns} { o_result = res }
    where
       fx        = def { FC.solver  = fromJust (smtsolver cfg)
                       , FC.real    = real   cfg
                       , FC.native  = native cfg
                       , FC.srcFile = target
                       -- , FC.stats   = True
                       }
       ferr s r  = fmap (tidyError s) $ result $ sinfo <$> r

--------------------------------------------------------------------------------
-- Complete LiquidHaskell Pipeline ---------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Rename (doesn't actually "exit")
exitWithResult :: Config -> FilePath -> Output Doc -> IO (Output Doc)
exitWithResult cfg target out
  = do {-# SCC "annotate" #-} annotate cfg target out
       donePhase Loud "annotate"
       writeCheckVars $ o_vars  out
       writeResult cfg (colorResult r) r
       writeFile (extFileName FF.Result target) (showFix r)
       return $ out { o_result = r }
    where
       r         = o_result out `addErrors` o_errors out


writeCheckVars Nothing     = return ()
writeCheckVars (Just [])   = colorPhaseLn Loud "Checked Binders: None" ""
writeCheckVars (Just ns)   = colorPhaseLn Loud "Checked Binders:" "" >> forM_ ns (putStrLn . symbolString . dropModuleNames . symbol)

writeResult cfg c          = mapM_ (writeDoc c) . zip [0..] . resDocs tidy
  where
    tidy                   = if shortErrors cfg then Lossy else Full
    writeDoc c (i, d)      = writeBlock c i $ lines $ render d
    writeBlock _ _ []      = return ()
    writeBlock c 0 ss      = forM_ ss (colorPhaseLn c "")
    writeBlock _  _ ss     = forM_ ("\n" : ss) putStrLn

resDocs _ Safe             = [text "SAFE"]
resDocs k (Crash xs s)     = text ("ERROR: " ++ s) : pprManyOrdered k "" (errToFCrash <$> xs)
resDocs k (Unsafe xs)      = text "UNSAFE" : pprManyOrdered k "" (nub xs)
resDocs _ (UnknownError d) = [text $ "PANIC: Unexpected Error: " ++ d, reportUrl]

reportUrl                  = text "Please submit a bug report at: https://github.com/ucsd-progsys/liquidhaskell"


addErrors r []             = r
addErrors Safe errs        = Unsafe errs
addErrors (Unsafe xs) errs = Unsafe (xs ++ errs)
addErrors r  _             = r

instance Fixpoint (FixResult Error) where
  toFix = vcat . resDocs Full

--------------------------------------------------------------------------------
--- Pretty Printing GhcInfo ----------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Move these elsewhere?

instance Show GhcInfo where
  show = showpp

instance PPrint GhcInfo where
  pprint info =   (text "*************** Imports *********************")
              $+$ (intersperse comma $ text <$> imports info)
              $+$ (text "*************** Includes ********************")
              $+$ (intersperse comma $ text <$> includes info)
              $+$ (text "*************** Imported Variables **********")
              $+$ (pprDoc $ impVars info)
              $+$ (text "*************** Defined Variables ***********")
              $+$ (pprDoc $ defVars info)
              $+$ (text "*************** Specification ***************")
              $+$ (pprint $ spec info)
              $+$ (text "*************** Core Bindings ***************")
              $+$ (pprint $ cbs info)

instance PPrint GhcSpec where
  pprint spec =  (text "******* Target Variables ********************")
              $$ (pprint $ tgtVars spec)
              $$ (text "******* Type Signatures *********************")
              $$ (pprintLongList $ M.toList $ tySigs spec)
              $$ (text "******* Type Synonyms ***********************")
              $$ (pprintLongList $ M.toList $ rtEnv spec)
              $$ (text "******* FTycon Embeds ***********************")
              $$ (pprintLongList $ map (second fTyconSymbol) $ M.toList $ tcEmbeds spec)
              $$ (text "******* Assumed Type Signatures *************")
              $$ (pprintLongList $ M.toList $ asmSigs spec)
              $$ (text "******* DataCon Specifications (Measure) ****")
              $$ (pprintLongList $ M.toList $ ctors spec)
              $$ (text "******* Measure Specifications **************")
              $$ (pprintLongList $ M.toList $ meas spec)
              $$ (text "******* Qualifiers *****************************")
              $$ (pprintLongList $ M.elems $ qualifiers spec)

instance PPrint [CoreBind] where
  pprint = pprDoc . tidyCBs

instance PPrint TargetVars where
  pprint AllVars   = text "All Variables"
  pprint (Only vs) = text "Only Variables: " <+> pprint vs


