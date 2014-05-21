{-# LANGUAGE ImplicitParams #-}
module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Distribution.Package
import Distribution.Text
import Distribution.Version
import System.Environment
import Text.Printf

import CabalFile
import VersionFile

import MaybeIO
import Search


-- | Some packages are "pinned" to a particular version by the system, and no
--   other versions may be installed. So don't try.
pinned_packages :: [String]
pinned_packages = [ "Cabal"
                  , "array"
                  , "base"
                  , "bin-package-db"
                  , "binary"
                  , "bytestring"
                  , "containers"
                  , "deepseq"
                  , "directory"
                  , "filepath"
                  , "ghc"
                  , "ghc-prim"
                  , "haskell2010"
                  , "haskell98"
                  , "hoopl"
                  , "hpc"
                  , "integer-gmp"
                  , "old-locale"
                  , "old-time"
                  , "pretty"
                  , "process"
                  , "rts"
                  , "template-haskell"
                  , "time"
                  , "unix"
                  ]

is_pinned :: PackageName -> Bool
is_pinned (PackageName name) = (name `elem` pinned_packages)


-- | With the given Cabal file, does the project build?
build_with_cabal :: (?cabal_file :: FilePath) => Cabal -> MaybeIO
build_with_cabal cabal = do
    lift $ writeCabal ?cabal_file cabal
    run "rm -rf dist cabal.sandbox.config .cabal-sandbox"
    run "cabal -v0 sandbox init"
    run "cabal -v0 install --only-dependencies"
    run "cabal -v0 build"
    lift $ putStrLn "cabal-rangefinder: OK"

-- | How about with the default Cabal plus a specific version constrain?
build_with_version :: (?cabal_file :: FilePath, ?cabal :: Cabal)
                   => PackageName -> Version
                   -> MaybeIO
build_with_version p v = do
    lift header
    build_with_cabal cabal'
  where
    header :: IO ()
    header = printf "cabal-rangefinder: trying %s %s\n" (display p) (display v)
    
    cabal' = ?cabal // [(p, thisVersion v)]

-- | Same, but with the type expected by binary_search.
builds_with_version :: (?cabal_file :: FilePath, ?cabal :: Cabal)
                    => PackageName -> Version
                    -> IO (Maybe [(PackageName, VersionRange)])
builds_with_version p v = runMaybeT
                        $ fmap (const [(p, orLaterVersion v)])
                        $ build_with_version p v

-- | Assuming the latest version works, how old can we go back?
find_first_version :: (?cabal_file :: FilePath, ?cabal :: Cabal)
                   => PackageName -> [Version]
                   -> IO [(PackageName, VersionRange)]
find_first_version p = binary_search []
                     . map (builds_with_version p)


getCabalPath :: IO FilePath
getCabalPath = do
    [cabal_file] <- getArgs
    return cabal_file

main :: IO ()
main = do
    cabal_file <- getCabalPath
    cabal <- readCabal cabal_file
    -- mapM_ print cabal
    
    let ?cabal_file = cabal_file
    let ?cabal = cabal
    
    putStrLn "cabal-rangefinder: trying original .cabal file"
    untouchedOk <- succeeds $ build_with_cabal cabal
    when (not untouchedOk) $ do
      error "You should at least start with a working .cabal file."
    
    versionMap <- readVersionMap
    -- mapM_ print versionMap
    
    let all_packages = packages cabal
    let our_packages = filter (not . is_pinned) all_packages
    -- mapM_ print our_packages
    
    let our_versions = versionMap `restricted_to` our_packages
    mapM_ print $ map (display *** map display) our_versions
    
    assignmentss <- forM our_versions $ \(p, vs) -> do
      printf "cabal-rangefinder: exploring %s\n" (display p) :: IO ()
      assignment <- find_first_version p vs
      when (null assignment) $ do
        printf "cabal-rangefinder: no good version for %s!\n" (display p) :: IO ()
      forM_ assignment $ \(p', v) ->
        printf "cabal-rangefinder: picked %s %s\n" (display p') (display v) :: IO ()
      return assignment
    print assignmentss
    let assignments = concat assignmentss
    -- assignments <- msum $ map (uncurry find_first_version) our_versions
    print assignments
    
    let final_cabal = cabal // assignments
    finalOk <- succeeds $ build_with_cabal final_cabal
    when (not finalOk) $ do
      error "I spent all this time building the perfect .cabal file, and it doesn't even run :("
