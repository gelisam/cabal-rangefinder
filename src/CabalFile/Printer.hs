module CabalFile.Printer where

import Distribution.Text
import System.IO

import CabalFile.Types


printCabal :: Handle -> Cabal -> IO ()
printCabal h cabal = mapM_ (hPutStr h) fragments
  where
    fragments :: [String]
    fragments = map (either id display) cabal

writeCabal :: FilePath -> Cabal -> IO ()
writeCabal path = withFile path WriteMode . flip printCabal
