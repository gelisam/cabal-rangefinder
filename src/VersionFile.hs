-- | Re-export the VersionFile.* submodules.
module VersionFile
  ( VersionMap
  , restricted_to
  
  , readVersionMap
  ) where

import VersionFile.Types
import VersionFile.Parser
