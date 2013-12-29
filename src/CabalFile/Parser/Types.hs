-- | The monad in which parseCabal is most easily expressed.
module CabalFile.Parser.Types where

import Control.Monad.State
import Control.Monad.Writer

import CabalFile.Types


-- | The string which remains to be parsed, the pieces which have already been
--   parsed, and a result. Or, if unsuccessful, Nothing.
type ParseC a = StateT String (WriterT Cabal Maybe) a
