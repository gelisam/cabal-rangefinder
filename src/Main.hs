module Main where

-- import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
-- import Distribution.Text
import System.Environment
import System.Exit
import System.Process

import CabalFile
import VersionFile


-- | The first action which succeeds, assuming all later action also do.
-- >>> binary_search 4 [[Nothing], [Nothing], [Just 2], [Just 3]]
-- [2]
-- 
-- >>> binary_search 4 [[Nothing], [Nothing], [Nothing], [Nothing]]
-- [4]
-- 
-- prop> let n = x1 `mod` 10; k = if n > 0 then x2 `mod` n else 0; xs = [[Nothing] | x <- [0..k-1]] ++ [[Just x] | x <- [k..n-1]] in binary_search n xs == [k]
binary_search :: Monad m => a -> [m (Maybe a)] -> m a
binary_search d [] = return d
binary_search d mxs = do
    r <- mx
    case r of
      Nothing -> binary_search d mxs2
      Just x  -> binary_search x mxs1
  where
    i = length mxs `div` 2
    (mxs1, mx:mxs2) = splitAt i mxs


-- | Does the given computation run until the end?
succeeds :: MaybeT IO () -> IO Bool
succeeds = fmap isJust . runMaybeT

-- | Does the given command-line program succeed?
run :: String -> MaybeT IO ()
run cmd = do
    exitCode <- lift $ system cmd
    when (exitCode /= ExitSuccess) $ do
      fail "command failed"

-- | With the given Cabal file, does the project build?
build :: FilePath -> Cabal -> MaybeT IO ()
build cabal_file cabal = do
    lift $ writeCabal cabal_file cabal
    run "cabal-dev install-deps"
    run "cabal-dev build"


getCabalPath :: IO FilePath
getCabalPath = do
    [cabal_file] <- getArgs
    return cabal_file

main :: IO ()
main = do
    cabal_file <- getCabalPath
    cabal <- readCabal cabal_file
    -- mapM_ print cabal
    
    untouchedOk <- succeeds $ build cabal_file cabal
    when (not untouchedOk) $ do
      error "You should at least start with a working .cabal file."
    
    versionMap <- readVersionMap
    -- mapM_ print versionMap
    
    let our_packages = packages cabal
    -- mapM_ print our_packages
    
    let our_versions = versionMap `restricted_to` our_packages
    -- mapM_ print $ map (display *** map display) our_versions
    
    finalOk <- succeeds $ build cabal_file cabal
    when (not finalOk) $ do
      error "I spent all this time building the perfect .cabal file, and it doesn't even run :("
