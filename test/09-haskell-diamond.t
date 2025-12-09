
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/09-haskell-diamond example

  $ jenga build -a
  A: echo 9.8.4 > version
  A: echo ~/.ghcup/bin/ghc-$(cat version) > ghc-path
  A: echo exec $(cat ghc-path) '"$@"' > ghc.exe
  A: chmod +x ghc.exe
  A: echo 'import Top' > main.hs
  A: ./ghc.exe -c A.hs
  A: ./ghc.exe -c B.hs
  A: ./ghc.exe -c C.hs
  A: ./ghc.exe -c Top.hs
  A: ./ghc.exe -c main.hs
  A: ./ghc.exe -o diamond.exe main.o Top.o B.o C.o A.o
  ran 11 commands
  checked 10 rules

  $ jenga exec example/diamond.exe
  Top[B[A],C[A]]

  $ jenga build -a --debug-demand
  B: Require: example/diamond.exe
  B: Require: example/ghc.exe
  B: Require: example/ghc-path
  B: Require: example/version
  B: Require: example/main.o
  B: Require: example/main.hs
  B: Require: example/Top.hi
  B: Require: example/B.hi
  B: Require: example/A.hi
  B: Require: example/C.hi
  B: Require: example/Top.o
  B: Require: example/B.o
  B: Require: example/C.o
  B: Require: example/A.o
  checked 10 rules
