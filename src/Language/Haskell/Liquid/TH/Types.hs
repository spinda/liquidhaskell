{-# LANGUAGE DeriveDataTypeable #-}

module Language.Haskell.Liquid.TH.Types (
    -- * Location Information
    SourceSpan(..)
  , spanLocated
  , locatedSpan

    -- * Annotation Types
  , LiquidVar(..)
  , LiquidSyn(..)
  , EmbedAs(..)
  , LiftToLogic(..)
  , LogicKind(..)
  ) where

import Data.Data
import Data.Typeable

import Language.Haskell.TH.Syntax (Name, mkName)

import Text.Parsec.Pos

import Language.Fixpoint.Types

import Language.Haskell.Liquid.Types

--------------------------------------------------------------------------------
-- Location Information --------------------------------------------------------
--------------------------------------------------------------------------------

data SourceSpan
  = SourceSpan
    { ss_start      :: SourcePos
    , ss_end        :: SourcePos
    } deriving (Data, Typeable)

spanLocated :: SourceSpan -> a -> Located a
spanLocated (SourceSpan start end) val = Loc start end val

locatedSpan :: Located a -> SourceSpan
locatedSpan (Loc start end _) = SourceSpan start end

--------------------------------------------------------------------------------
-- Annotation Types ------------------------------------------------------------
--------------------------------------------------------------------------------

data LiquidVar
  = LiquidVar
    { lv_full       :: !Bool
    , lv_assumed    :: !Bool
    , lv_type       :: !(Located AnnoType)
    , lv_span       :: !SourceSpan
    }
 deriving (Data, Typeable)

data LiquidSyn
  = LiquidSyn
    { ls_exprParams :: ![String]
    , ls_rhs        :: !(Located AnnoType)
    , ls_span       :: !SourceSpan
    }
  deriving (Data, Typeable)

data EmbedAs
  = EmbedAs
    { ea_ftc        :: !(Located FTycon)
    , ea_span       :: !SourceSpan
    }
  deriving (Data, Typeable)

data LiftToLogic
  = LiftToLogic
    { ltl_kind      :: !LogicKind
    , ltl_span      :: !SourceSpan
    }
 deriving (Data, Typeable)

data LogicKind
  = InlineKind
  | BoundKind
  | MeasureKind
  | QualifKind
  deriving (Data, Typeable)

