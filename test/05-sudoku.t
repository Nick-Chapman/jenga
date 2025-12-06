
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/05-sudoku example

  $ jenga build -a
  A: echo 9.8.4 > version
  A: echo ~/.ghcup/bin/ghc-$(cat version) > ghc-path
  A: echo exec $(cat ghc-path) '"$@"' > ghc.exe
  A: chmod +x ghc.exe
  A: ./ghc.exe -c Sudoku.hs -XLambdaCase
  A: ./ghc.exe -c main.hs
  A: ./ghc.exe main.o Sudoku.o -package containers -o solver.exe
  checked 7 targets
  ran 7 commands

  $ jenga exec example/solver.exe example/puzzle
  ...3.9..5
  ...475...
  ......4..
  89.......
  .7..5..89
  56....3.4
  ..7..492.
  ....9.6.8
  2...31.4.
  
  428369715
  913475862
  756812493
  892743156
  374156289
  561928374
  637584921
  145297638
  289631547
  
  1

