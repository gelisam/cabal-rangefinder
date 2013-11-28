module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad.Writer
import Data.Functor
import Data.List
import Data.Maybe
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity
import System.Environment
import System.IO
import Text.PrettyPrint hiding (empty)
import Text.Printf


-- unlike Distribution.PackageDescription, we keep the whitespace.
type Cabal = [Either String Dependency]

-- | Parse a .cabal file by writing it piece by piece.
type ParseC a = WriterT Cabal Maybe a

-- | Print the parsed pieces, simplifies tests.
testC :: Show a => ParseC a -> IO ()
testC = go . runWriterT
  where
    go Nothing = putStrLn "Nothing"
    go (Just (x, ys)) = do
      mapM_ putStrLn $ map show' ys
      print x
    show' (Left s) = show s
    show' (Right d) = printf "[%s]" $ render $ disp d

-- | Write a String, which we won't modify further.
-- >>> testC $ stringC "foo"
-- "foo"
-- ()
stringC :: String -> ParseC ()
stringC s = tell [Left s]

-- | Parse and write a Dependency, which we will modify later.
-- >>> testC $ dependencyC "containers"
-- [containers -any]
-- ()
-- 
-- >>> testC $ dependencyC "bogus text"
-- Nothing
dependencyC :: String -> ParseC ()
dependencyC s = do
    d <- lift $ simpleParse s
    tell [Right d]


-- | In general, a fooC function performs foo in the ParseC monad,
--   but also writes whatever was consumed.

-- | Change, but also write fst.
-- >>> testC $ mapFstC length ("foo", 42)
-- "foo"
-- (3,42)
mapFstC :: (String -> a) -> (String, b) -> ParseC (a, b)
mapFstC f (s, x) = stringC s >> return (f s, x)

-- | Like span, but also write fst.
-- >>> testC $ spanC (== 'a') "aaA:Aaa"
-- "aa"
-- ("aa","A:Aaa")
spanC :: (Char -> Bool) -> String -> ParseC (String, String)
spanC p = mapFstC id . span p

-- | Like break, but also write fst.
-- >>> testC $ breakC (== ':') "aaA:Aaa"
-- "aaA"
-- ("aaA",":Aaa")
breakC :: (Char -> Bool) -> String -> ParseC (String, String)
breakC p = mapFstC id . break p

-- | Like snd, but also write fst.
-- >>> testC $ sndC ("foo",42)
-- "foo"
-- 42
sndC :: (String, a) -> ParseC a
sndC = fmap snd . mapFstC id

-- | Like dropWhile, but also write the dropped part.
dropWhileC :: (Char -> Bool) -> String -> ParseC String
dropWhileC p = sndC . span p

dropWhitespaceC :: String -> ParseC String
dropWhitespaceC = dropWhileC (`elem` " \t")

-- | Like isPrefixOf, but also write the prefix and return the remainder.
-- >>> testC $ "abc" `isPrefixOfC` "abcdef"
-- "abc"
-- "def"
-- 
-- >>> testC $ "abc" `isPrefixOfC` "ABCDEF"
-- Nothing
isPrefixOfC :: String -> String -> ParseC String
isPrefixOfC prefix s = do
    guard $ prefix `isPrefixOf` s
    stringC prefix
    return $ drop (length prefix) s


-- | Write and measure the indentation.
-- 
-- >>> fst $ splitIndent "  hello"
-- 2
-- 
-- >>> testC $ snd $ splitIndent "  hello"
-- "  "
-- "hello"
splitIndent :: String -> (Int, ParseC String)
splitIndent = go . span (`elem` " \t")
  where
    go (i, s) = (length i, stringC i >> return s)

-- | Extract an indented block, indented by more than i chars.
-- 
-- >>> splitUnindent 2 [(4,"A"),(4,"B"),(6,"B1"),(6,"B2"),(4,"C"),(2,"else")]
-- (["A","B","B1","B2","C"],[(2,"else")])
splitUnindent :: Int -> [(Int, a)] -> ([a], [(Int, a)])
splitUnindent i = (map snd *** id) . span isMoreIndented
  where
    isMoreIndented = (> i) . fst


parseCabal :: String -> Cabal
parseCabal = fromJust . execWriterT . go . map splitIndent . lines
  where
    go :: [(Int, ParseC String)] -> ParseC ()
    go [] = return ()
    go ((i, get_line):xs) = do
        line <- get_line
        build_depends i line xs <|> one_line line xs
    
    one_line :: String -> [(Int, ParseC String)] -> ParseC ()
    one_line s xs = do
        stringC s
        stringC "\n"
        go xs
    
    build_depends :: Int -> String -> [(Int, ParseC String)] -> ParseC ()
    build_depends i s xs = do
        s <- "build-depends" `isPrefixOfC` s
        s <- dropWhitespaceC s
        s <- ":" `isPrefixOfC` s
        s <- dropWhitespaceC s
        let (cs, xs') = splitUnindent i xs
        dependencies s cs
        go xs'
    
    dependencies :: String -> [ParseC String] -> ParseC ()
    dependencies "" [] = empty
    dependencies "" (c:cs) = do
        stringC "\n"
        s <- c
        dependencies s cs
    dependencies s cs = do
        let (s1, s2) = break (== ',') s
        dependencyC s1
        comma s2 cs
    
    comma :: String -> [ParseC String] -> ParseC ()
    comma "" [] = stringC "\n"
    comma "" (c:cs) = do
        stringC "\n"
        s <- c
        comma s cs
    comma s cs = do
        s <- "," `isPrefixOfC` s
        s <- dropWhitespaceC s
        dependencies s cs


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
