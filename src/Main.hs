module Main where

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import System.Environment


main :: IO ()
main = do
    [cabal_file] <- getArgs
    d <- readPackageDescription normal cabal_file
    writePackageDescription cabal_file (packageDescription d)
