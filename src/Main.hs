module Main where

import Control.Applicative
import Distribution.Text
import System.Environment
import System.IO
import Text.PrettyPrint hiding (empty)

import Parse


-- like readFile, but without the lazy IO
readFile' :: FilePath -> IO String
readFile' f = do
    s <- readFile f
    length s `seq` return s

main :: IO ()
main = do
    [cabal_file] <- getArgs
    cabal <- parseCabal <$> readFile' cabal_file
    -- mapM_ print cabal
    length cabal_file `seq` withFile cabal_file WriteMode $ \h -> do
      mapM_ (hPutStr h) $ map (either id (render . disp)) cabal
