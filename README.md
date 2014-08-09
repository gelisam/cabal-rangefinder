cabal-rangefinder
=================

A tool to fill in the version ranges in a cabal file.

purpose
-------

Suppose you have a Haskell project which builds fine on your machine with `cabal install`. Will it also build fine on other machines?

One common reason for build failures is unspecified version ranges.

    build-depends:  base >= 4 && < 5,
                    containers,
                    mtl

But how do you know which versions of each package your code is compatible with? Unless you are very familiar with the history of each package, your only choice is to test with many versions. Cabal-rangefinder automates the process.

usage
-----

Simply call `cabal-rangefinder myproject.cabal`, and wait while your program gets compiled with every version of every dependency.

correctness
-----------

We assume that the current cabal file already compiles fine with the latest version of everything. That is, you should use cabal-rangefinder to expand the lower end of your version ranges, not to find a version which works in the first place.

We also assume that if versions X and Y both work, then so do all the versions in between.
