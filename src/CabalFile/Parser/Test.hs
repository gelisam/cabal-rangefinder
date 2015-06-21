-- | Simplify doctests by pretty-printing results.
{-# LANGUAGE ScopedTypeVariables #-}
module CabalFile.Parser.Test where

import Control.Monad.State
import Control.Monad.Writer
import Distribution.Text
import Distribution.Package
import Text.Printf

import CabalFile.Parser.Types
import CabalFile.Types


-- | Print the parsed pieces, the result, and the remaining string.
-- >>> testC "foo" (return 42)
-- 42
-- "foo"
testC :: forall a. Show a => String -> ParseC a -> IO ()
testC doc = go . runWriterT . (`runStateT` doc)
  where
    go :: Maybe ((a, String), Cabal) -> IO ()
    go Nothing = putStrLn "Nothing"
    go (Just ((x, s), ys)) = do
      mapM_ putStrLn $ map show' ys
      print x
      unless (null s) $ print s
    
    show' :: Either String Dependency -> String
    show' (Left s) = show s
    show' (Right d) = printf "[%s]" $ display d
