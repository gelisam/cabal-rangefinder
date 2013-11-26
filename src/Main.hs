module Main where

import Data.Functor
import Data.List
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity
import System.Environment
import System.IO
import Text.PrettyPrint


-- | Separate the indentation from the contents.
-- 
-- >>> splitIndent "  hello"
-- ("  ","hello")
splitIndent :: String -> (String, String)
splitIndent = span (`elem` " \t")

-- | Extract the first line's indented block.
-- 
-- >>> splitUnindent [(" ","1"), ("  ","1.1"), ("  ","1.2"), (" ","2")]
-- ([("","1"),("  ","1.1"),("  ","1.2")],[(" ","2")])
splitUnindent :: [(String, String)] -> ([(String,String)], [(String, String)])
splitUnindent [] = ([], [])
splitUnindent ((indent, x):ixs) = (("",x):ixs1, ixs2)
  where
    (ixs1, ixs2) = span isMoreIndented ixs
    isMoreIndented = (> length indent) . length . fst


-- unlike Distribution.PackageDescription, we keep the whitespace.
type Cabal = [Either String Dependency]

parseCabal :: String -> Cabal
parseCabal = go . map splitIndent . lines
  where
    go ixs | any (has_marker . snd) ixs
        = concatMap mergeIndent ixs1
       ++ [Left i1, Left marker_stuff]
       ++ concatMap dependency lines
       ++ go ixs3
      where
        (ixs1, (i1,marker_line):ixs2) = break (has_marker . snd) ixs
        (marker_stuff, x1) = break_colon marker_line
        (lines, ixs3) = splitUnindent ((i1,x1):ixs2)
        dependency (i,x) = case simpleParse x1 of
            Nothing -> [Left i, Left x, Left "\n"]
            Just d -> [Left i, Right d, Left x2, Left "\n"]
          where
            (x1,x2) = break (==',') x
    go ixs = concatMap mergeIndent ixs
    marker = "build-depends"
    has_marker x = marker `isPrefixOf` x
                && ":" `isPrefixOf` post_marker x
    post_marker = dropWhile (`elem` " \t") . drop (length marker)
    break_marker x = (m ++ whitespace, rest')
      where
        (m, rest) = splitAt (length marker) x
        (whitespace, rest') = span (`elem` " \t") rest
    break_colon x = (m ++ colon, rest')
      where
        (m, rest) = break_marker x
        (colon, rest') = span (`elem` ": \t") rest
    mergeIndent (x, x') = [Left x, Left x', Left "\n"]

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
