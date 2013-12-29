module Main where

import Control.Applicative
import Distribution.Package
import Distribution.Simple.PackageIndex
import Distribution.Text
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.PrettyPrint hiding (empty)

import Parse


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

getCachePath :: IO FilePath
getCachePath = do
    home <- getHomeDirectory
    return $ home </> ".cabal/packages/hackage.haskell.org/00-index.cache"

main :: IO ()
main = do
    [cabal_file] <- getArgs
    version_file <- getCachePath
    cabal <- parseCabal <$> readFile' cabal_file
    versionMap <- parseVersionMap <$> readFile' version_file
    -- mapM_ print cabal
    -- mapM_ print versionMap
    withFile cabal_file WriteMode $ \h -> do
      mapM_ (hPutStr h) $ map (either id (render . disp)) cabal
