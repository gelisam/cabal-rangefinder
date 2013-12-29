-- | Simplify doctests by pretty-printing results.
module CabalFile.Parser.Test where

import Control.Monad.State
import Control.Monad.Writer
import Distribution.Text
import Text.Printf

import CabalFile.Parser.Types


-- | Print the parsed pieces, the result, and the remaining string.
-- >>> testC "foo" (return 42)
-- 42
-- "foo"
testC :: Show a => String -> ParseC a -> IO ()
testC doc = go . runWriterT . (`runStateT` doc)
  where
    go Nothing = putStrLn "Nothing"
    go (Just ((x, s), ys)) = do
      mapM_ putStrLn $ map show' ys
      print x
      unless (null s) $ print s
    show' (Left s) = show s
    show' (Right d) = printf "[%s]" $ display d
