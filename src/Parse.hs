module Parse where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe

import ParseC


parseCabal :: String -> Cabal
parseCabal = fromJust . execWriterT . execStateT cabal
  where
    cabal :: ParseC ()
    cabal = eofC <|> ((build_depends <|> lineC) >> cabal)
    
    build_depends :: ParseC ()
    build_depends = do
        i <- indentC
        stringC "build-depends"
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
        go = do
            eolC
            j <- indentC
            go <|> guard (j > i)
