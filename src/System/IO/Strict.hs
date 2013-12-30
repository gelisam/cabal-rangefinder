-- | Lazy I/O isn't that bad, but we need to re-write the "package-name.cabal"
--   file, and for this we cannot allow the runtime to start writing before we
--   are done reading the old contents.
module System.IO.Strict where


-- like readFile, but without the lazy IO
readFile' :: FilePath -> IO String
readFile' f = do
    s <- readFile f
    length s `seq` return s
