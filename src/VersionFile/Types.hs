-- | To represent a version map,
--   that is, which version numbers exist for each package.
module VersionFile.Types where

import Distribution.Package (PackageName)
import Distribution.Version (Version)


-- A simple association list. Profile before you optimize :)
type VersionMap = [(PackageName, [Version])]
