-- | To represent a "package-name.cabal" file.
--   We only care about the dependencies, but we also need to preserve
--   everything else (including the whitespace!) because we will write the file
--   back to disk and we don't want to obliterate the user's indentation style.
module CabalFile.Types where

import Data.Either
import Distribution.Package
import Distribution.Version


-- The Cabal library already has the type Distribution.PackageDescription, and
-- it already has a parser and a pretty-printer. However, we cannot use that
-- representation because we need to keep the whitespace information intact.
type Cabal = [Either String Dependency]

dependencies :: Cabal -> [Dependency]
dependencies = rights

packages :: Cabal -> [PackageName]
packages = map package . dependencies
  where
    package :: Dependency -> PackageName
    package (Dependency p _) = p

-- Replace the given dependencies
(//) :: Cabal -> [(PackageName, VersionRange)] -> Cabal
[] // _ = []
(Left s:xs) // ds = Left s : (xs // ds)
(Right (Dependency p v):xs) // ds = case lookup p ds of
    Nothing -> Right (Dependency p v)  : (xs // ds)
    Just v' -> Right (Dependency p v') : (xs // ds)
