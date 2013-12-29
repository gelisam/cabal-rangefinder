-- | The monad in which Parse.parseVersionMap can be written easily.
module ParseV where

import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import Distribution.Package (PackageName)
import Distribution.Text
import Distribution.Version (Version)


-- A simple association list. Profile before you optimize :)
type VersionMap = [(PackageName, [Version])]

-- A much simpler monad stack than for ParseC :)
type ParseV a = String -> Maybe a

-- | Print the parsed piece, simplifies tests.
-- >>> testV id $ return 4
-- 4
testV :: Show s => (a -> s) -> Maybe a -> IO ()
testV _ Nothing = putStrLn "Nothing"
testV show' (Just x) = print (show' x)

-- | More testing, laying out each item on a separate line.
-- >>> testVs id $ return [1, 2, 3]
-- 1
-- 2
-- 3
testVs :: Show s => (a -> s) -> Maybe [a] -> IO ()
testVs _ Nothing = putStrLn "Nothing"
testVs show' (Just xs) = mapM_ (print . show') xs

-- |
-- >>> testV id (packageName "xturtle")
-- PackageName "xturtle"
packageName :: ParseV PackageName
packageName = simpleParse

-- |
-- >>> testV display (version "1.0.7")
-- "1.0.7"
version :: ParseV Version
version = simpleParse

-- |
-- >>> testV (display *** display) (pkg "pkg: xturtle 0.0.7 b# 211609")
-- ("xturtle","0.0.7")
pkg :: ParseV (PackageName, Version)
pkg s = do
    "pkg:":p:v:_ <- return $ words s
    p' <- packageName p
    v' <- version v
    return (p', v')

-- |
-- >>> testVs (display *** display) (pkgs "pref-ver: HSlippyMap 0.1.0.0\npkg: xtest 0.2 b# 211591\npkg: xturtle 0.0.1 b# 211594\npkg: xturtle 0.0.2 b# 211597\n")
-- ("xtest","0.2")
-- ("xturtle","0.0.1")
-- ("xturtle","0.0.2")
pkgs :: ParseV [(PackageName, Version)]
pkgs = Just . mapMaybe pkg . lines

-- |
-- -- >>> testVs (display *** map display) (pkgs "pref-ver: HSlippyMap 0.1.0.0\npkg: xtest 0.2 b# 211591\npkg: xturtle 0.0.1 b# 211594\npkg: xturtle 0.0.2 b# 211597\n")
-- -- ("xtest",["0.2"])
-- -- ("xturtle",["0.0.1","0.0.2"])
-- 
versionMap :: ParseV VersionMap
versionMap = fmap merge . pkgs
  where
    merge :: [(PackageName, Version)] -> [(PackageName, [Version])]
    merge = map (package &&& versions) . groupBy same_package
    
    same_package = (==) `on` fst
    package = fst . head
    versions = map snd
