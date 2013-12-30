module Main where

-- import Control.Arrow
import Control.Monad
import Control.Monad.Trans
-- import Distribution.Text
import System.Environment

import CabalFile
import VersionFile

import MaybeIO
import Search


-- | With the given Cabal file, does the project build?
build :: FilePath -> Cabal -> MaybeIO
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
