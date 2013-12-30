-- | IO computations which can fail without crashing the program.
module MaybeIO where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Exit
import System.Process


type MaybeIO = MaybeT IO ()

-- | Does the given computation run until the end?
succeeds :: MaybeIO -> IO Bool
succeeds = fmap isJust . runMaybeT

-- | Does the given command-line program succeed?
run :: String -> MaybeIO
run cmd = do
    exitCode <- lift $ system cmd
    when (exitCode /= ExitSuccess) $ do
      fail "command failed"
