-- | Re-export the CabalFile.* submodules.
module CabalFile
  ( Cabal
  , dependencies
  , packages
  , (//)
  
  , readCabal
  , writeCabal
  ) where

import CabalFile.Types
import CabalFile.Parser
import CabalFile.Printer
