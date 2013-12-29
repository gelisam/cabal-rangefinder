-- | The monad in which parseVersionMap is most easily expressed.
module VersionFile.Parser.Types where


-- A much simpler monad stack than for CabalFile.Parser :)
type ParseV a = String -> Maybe a
