-- | To parse the version map data hidden in cabal's local cache file,
--   located at "~/.cabal/packages/hackage.haskell.org/00-index.cache".
--   It looks like this, but much longer:
--   
--     pref-ver: imagemagick >=0.0.2
--     pkg: imagemagick 0.0.1 b# 123041
--     pkg: imagemagick 0.0.2 b# 123078
--     pkg: imagemagick 0.0.3 b# 123116
--     pkg: imagemagick 0.0.3.1 b# 123154
--     pkg: mtl 1.0 b# 143620
--     pkg: mtl 1.1.0.0 b# 143623
--     pkg: mtl 1.1.0.1 b# 143627
--     pkg: mtl 1.1.0.2 b# 143631
--     pkg: mtl 1.1.1.0 b# 143635
--     pkg: mtl 1.1.1.1 b# 143639
--     pkg: mtl 2.0.0.0 b# 143643
--     pkg: mtl 2.0.1.0 b# 143647
--     pkg: mtl 2.1 b# 143651
--     pkg: mtl 2.1.1 b# 143655
--     pkg: mtl 2.1.2 b# 143659
--   
--   I assume that the "pref-ver" part remembers the version ranges which were
--   requested by the user on the command-line, while the "pkg" part remembers
--   which versions of each package are available. This mapping between each
--   package name and its available versions is what we call a version map.
--   
--   I don't know what the "b# 123456" part means. We ignore it.
module VersionFile.Parser
  ( getVersionPath
  , readVersionMap
  , parseVersionMap
  ) where

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe
import Distribution.Package (PackageName)
import Distribution.Text
import Distribution.Version (Version)
import System.Directory
import System.FilePath

import VersionFile.Types
import VersionFile.Parser.Test ()
import VersionFile.Parser.Types

-- $setup
-- >>> import VersionFile.Parser.Test


getVersionPath :: IO FilePath
getVersionPath = do
    home <- getHomeDirectory
    return $ home </> ".cabal/packages/hackage.haskell.org/00-index.cache"


-- |
-- >>> testV id (packageName "xturtle")
-- PackageName {unPackageName = "xturtle"}
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
    
    same_package :: (PackageName, Version) -> (PackageName, Version) -> Bool
    same_package = (==) `on` fst
    
    package :: [(PackageName, Version)] -> PackageName
    package = fst . head
    
    versions :: [(PackageName, Version)] -> [Version]
    versions = map snd


parseVersionMap :: String -> VersionMap
parseVersionMap = fromJust . versionMap

readVersionMap :: IO VersionMap
readVersionMap = do
    path <- getVersionPath
    parseVersionMap <$> readFile path
