-- | To parse a "package-name.cabal" file.
{-# LANGUAGE ScopedTypeVariables #-}
module CabalFile.Parser
  ( parseCabal
  , readCabal
  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Distribution.Text

import CabalFile.Types
import CabalFile.Parser.Test ()
import CabalFile.Parser.Types
import System.IO.Strict

-- $setup
-- >>> import CabalFile.Parser.Test


splitConsume :: (Functor m, Monad m)
             => (s -> (e, s))
             -> (e -> m a)
             -> StateT s m a
splitConsume split consume = do
    (e, s) <- split <$> get
    put s
    lift $ consume e

splitXform :: forall a. (String -> (String, String)) -> (String -> a) -> ParseC a
splitXform split f = splitConsume split go
  where
    go :: String -> WriterT Cabal Maybe a
    go s = do
        tell [Left s]
        return $ f s

splitIgnore :: (String -> (String, String)) -> ParseC ()
splitIgnore = (`splitXform` const ())

splitWhitespace :: String -> (String, String)
splitWhitespace = span (`elem` " \t")


-- | Consume the string if it matches the head of the input stream.
-- >>> testC "fool" $ stringC "foo"
-- "foo"
-- ()
-- "l"
-- 
-- >>> testC "bard" $ stringC "foo"
-- Nothing
-- 
-- >>> testC "bard" $ stringC "foo" <|> stringC "bar"
-- "bar"
-- ()
-- "d"
stringC :: String -> ParseC ()
stringC expected = splitConsume split check
  where
    split :: String -> (String, String)
    split = splitAt $ length expected
    
    check :: String -> WriterT Cabal Maybe ()
    check actual = do
        guard $ actual == expected
        tell [Left actual]

-- | Consume a dependency if it matches the head of the input stream.
-- >>> testC "containers" dependencyC
-- [containers -any]
-- ()
-- 
-- >>> testC "containers, mtl" dependencyC
-- [containers -any]
-- ()
-- ", mtl"
-- 
-- >>> testC "bogus text" dependencyC
-- Nothing
dependencyC :: ParseC ()
dependencyC = splitConsume split check
  where
    split :: String -> (String, String)
    split = break (`elem` ",\n")
    
    check :: String -> WriterT Cabal Maybe ()
    check s = do
        d <- lift $ simpleParse s
        tell [Right d]


-- | Only succeed if the file has been completely consumed.
-- >>> testC "" eofC
-- ()
-- 
-- >>> testC "more" eofC
-- Nothing
eofC :: ParseC ()
eofC = do
    s <- get
    guard $ null s

-- | Only succeed if the line has been completely consumed.
-- >>> testC "\n" eolC
-- "\n"
-- ()
-- 
-- >>> testC "more\n" eolC
-- Nothing
eolC :: ParseC ()
eolC = stringC "\n"


-- | Consume and ignore whitespace.
-- >>> testC "  hello" whitespaceC
-- "  "
-- ()
-- "hello"
whitespaceC :: ParseC ()
whitespaceC = splitIgnore splitWhitespace

-- | Consume and ignore the rest of the line.
-- >>> testC "hello" lineC
-- "hello"
-- ()
-- 
-- >>> testC "hello\nworld\n" lineC
-- "hello"
-- "\n"
-- ()
-- "world\n"
lineC :: ParseC ()
lineC = do
  splitIgnore $ break (== '\n')
  stringC "\n" <|> return ()

-- | Consume and measure the indentation.
-- >>> testC "  hello" indentC
-- "  "
-- 2
-- "hello"
indentC :: ParseC Int
indentC = splitXform splitWhitespace length


parseCabal :: String -> Cabal
parseCabal = fromJust . execWriterT . execStateT cabal
  where
    cabal :: ParseC ()
    cabal = eofC <|> ((build_depends <|> lineC) >> cabal)
    
    build_depends :: ParseC ()
    build_depends = do
        i <- indentC
        stringC "build-depends" <|> stringC "Build-Depends"
                                <|> stringC "Build-depends"
        whitespaceC
        stringC ":"
        dependency i
    
    dependency :: Int -> ParseC ()
    dependency i = do
        spacing i
        dependencyC
        (spacing i >> stringC "," >> dependency i) <|> eolC
    
    -- whitespace, including newlines, but stay withing the block of
    -- data which is indented by more than i characters.
    spacing :: Int -> ParseC ()
    spacing i = do
        whitespaceC
        go <|> return ()
      where
        go :: ParseC ()
        go = do
            eolC
            j <- indentC
            go <|> guard (j > i)

readCabal :: FilePath -> IO Cabal
readCabal = fmap parseCabal . readFile'
