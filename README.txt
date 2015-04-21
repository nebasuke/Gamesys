Solution to a coding exercise requiring to compute a series. 
See the DotNETCodingTest.pdf for a description of the problem.

To install:
- Install the Haskell platform
- Put all the contents of this repository in a directory.
- run cabal update followed by cabal install
- run gamesys executable (making sure that the cabal bin is in the PATH)
- To run the tests:
  + Run ghci src/Tests.hs (or src\Tests.hs)
  + try quickCheck5000 on 