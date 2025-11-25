
  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../src/jenga.exe jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/10-haskell-diamond-auto-deps example

  $ jenga build -a
  A: echo 9.8.4 > version
  A: echo ~/.ghcup/bin/ghc-$(cat version) > ghc-path
  A: echo exec $(cat ghc-path) '"$@"' > ghc.exe
  A: chmod +x ghc.exe
  A: echo 'import Top' > main.hs
  A: ./ghc.exe -M *.hs -dep-makefile depends
  A: ./ghc.exe -c A.hs
  A: ./ghc.exe -c C.hs
  A: ./ghc.exe -c B.hs
  A: ./ghc.exe -c Top.hs
  A: ./ghc.exe -c main.hs
  A: ./ghc.exe main.o Top.o B.o C.o A.o -o diamond.exe
  checked 16 targets
  ran 12 commands

  $ jenga exec example/diamond.exe
  Top[B[A],C[A]]

  $ jenga build -a --debug-demand
  B: Require: example/ghc.exe
  B: Require: example/ghc-path
  B: Require: example/version
  B: Require: example/depends
  B: Require: example/main.hs
  B: Require: example/Top.hi
  B: Require: example/C.hi
  B: Require: example/A.hi
  B: Require: example/B.hi
  B: Require: example/Top.o
  B: Require: example/C.o
  B: Require: example/B.o
  B: Require: example/A.o
  B: Require: example/main.hi
  B: Require: example/main.o
  B: Require: example/diamond.exe
  checked 16 targets
