
  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../,jenga/src/jenga jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/09-haskell-diamond example

  $ jenga build -a
  A: find $HOME/.stack | grep -v lib | grep bin/ghc$ | sort -n | tail -1 > ghc-path
  A: # Make use of multi-line actions and comments in actions...
  A: echo exec $(cat ghc-path) '"$@"' > ghc.exe
  A: chmod +x ghc.exe
  A: ./ghc.exe -c A.hs
  A: ./ghc.exe -c B.hs
  A: ./ghc.exe -c C.hs
  A: ./ghc.exe -c Top.hs
  A: echo 'import Top' > main.hs
  A: ./ghc.exe -c main.hs
  A: ./ghc.exe -o diamond.exe main.o Top.o B.o C.o A.o
  checked 13 targets
  ran 11 commands

  $ ,jenga/example/diamond.exe
  Top[B[A],C[A]]

  $ jenga build -a --debug-demand
  B: Require: example/ghc-path
  B: Require: example/ghc.exe
  B: Require: example/Top.hi
  B: Require: example/B.hi
  B: Require: example/A.hi
  B: Require: example/C.hi
  B: Require: example/Top.o
  B: Require: example/C.o
  B: Require: example/B.o
  B: Require: example/A.o
  B: Require: example/main.hs
  B: Require: example/main.o
  B: Require: example/diamond.exe
  checked 13 targets
