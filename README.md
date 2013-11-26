cabal-rangefinder
=================

A tool to fill in the version ranges in a cabal file.

(work in progress. this readme reflect planned, not present functionnality)

purpose
-------

Suppose you have a Haskell project which builds fine on your machine with `cabal-dev install`. Will it also build fine on other machines?

One common reason for build failures is unspecified version ranges.

    build-depends:  base >= 4 && < 5,
                    containers,
                    mtl

But how do you know which versions of each package your code is compatible with? Unless you are very familiar with the history of each package, your only choice is to test with many versions. cabal-rangefinder automates the process.

usage
-----

Simply call `cabal-rangefinder myproject.cabal`, and wait while your program gets compiled with every version of every dependency. Only the dependencies which do not specify any version range are investigated.

correctness
-----------

We assume that the versions currently installed in ./cabal-dev already work fine. That is, you should use cabal-rangefinder to expand your version ranges, not to find a version which works in the first place.

We also assume that if versions X and Y both work, then so does all the versions in between.
