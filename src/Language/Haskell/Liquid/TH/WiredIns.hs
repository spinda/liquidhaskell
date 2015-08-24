module Language.Haskell.Liquid.TH.WiredIns (
    -- * Wired-In Constants
    lqTypeParsePrefix

    -- * Wired-In Names
  , isWiredInName
  , cxtArrowTcName
  , funArrowTcName
  , tupleTcName
  , listTcName
  , equalityTcName
  ) where

import Language.Haskell.TH.Syntax

--------------------------------------------------------------------------------
-- Wired-in Constants ----------------------------------------------------------
--------------------------------------------------------------------------------

lqTypeParsePrefix :: String
lqTypeParsePrefix = "__liquid_type_attach__|"

--------------------------------------------------------------------------------
-- Wired-In Names --------------------------------------------------------------
--------------------------------------------------------------------------------

isWiredInName :: Name -> Bool
isWiredInName name = elem name
  [ cxtArrowTcName
  , funArrowTcName
  , tupleTcName
  , listTcName
  , equalityTcName
  ]


cxtArrowTcName, funArrowTcName, tupleTcName, listTcName, equalityTcName :: Name
cxtArrowTcName = mkName "=>"
funArrowTcName = mkName "->"
tupleTcName    = mkName "()"
listTcName     = mkName "[]"
equalityTcName = mkName "~"

