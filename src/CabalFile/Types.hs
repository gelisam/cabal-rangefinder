-- | To represent a "package-name.cabal" file.
--   We only care about the dependencies, but we also need to preserve
--   everything else (including the whitespace!) because we will write the file
--   back to disk and we don't want to obliterate the user's indentation style.
module CabalFile.Types where

import Distribution.Package


-- The Cabal library already has the type Distribution.PackageDescription, and
-- it already has a parser and a pretty-printer. However, we cannot use that
-- representation because we need to keep the whitespace information intact.
type Cabal = [Either String Dependency]
