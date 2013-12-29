-- | Simplify doctests by pretty-printing results.
module VersionFile.Parser.Test where


-- | Print the result if there is one.
-- >>> testV id $ return 4
-- 4
testV :: Show s => (a -> s) -> Maybe a -> IO ()
testV _ Nothing = putStrLn "Nothing"
testV show' (Just x) = print (show' x)

-- | Print each result on a separate line.
-- >>> testVs id $ return [1, 2, 3]
-- 1
-- 2
-- 3
testVs :: Show s => (a -> s) -> Maybe [a] -> IO ()
testVs _ Nothing = putStrLn "Nothing"
testVs show' (Just xs) = mapM_ (print . show') xs
