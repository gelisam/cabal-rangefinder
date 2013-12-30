module Main where

import Control.Applicative
-- import Control.Arrow
import Data.Either
import Distribution.Package
import Distribution.Simple.PackageIndex
import System.Environment

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


-- like readFile, but without the lazy IO
readFile' :: FilePath -> IO String
readFile' f = do
    s <- readFile f
    length s `seq` return s

printIt :: PackageIndex -> Either String Dependency -> IO ()
printIt _ (Left s) = print s
printIt p (Right d) = print $ lookupDependency p d

getCabalPath :: IO FilePath
getCabalPath = do
    [cabal_file] <- getArgs
    return cabal_file

main :: IO ()
main = do
    cabal_file <- getCabalPath
    cabal <- parseCabal <$> readFile' cabal_file
    -- mapM_ print cabal
    
    version_file <- getVersionPath
    versionMap <- parseVersionMap <$> readFile' version_file
    -- mapM_ print versionMap
    
    let package (Dependency p _) = p
    let our_packages = map package $ rights cabal
    -- mapM_ print our_packages
    
    let is_ours = (`elem` our_packages)
    let our_versions = filter (is_ours . fst) versionMap
    -- mapM_ print $ map (display *** map display) our_versions
    
    writeCabal cabal_file cabal
