
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/06-diamond example

  $ jenga build -a
  A: echo -n A > a
  A: echo -n B  > b
  A: echo -n C > c
  A: cat a b > ab
  A: cat b c > bc
  A: echo $(cat ab bc) > top
  ran 6 commands
  checked 6 rules

  $ jenga build -mq && cat ,jenga/example/top
  ABBC

The example has a diamond dependency on target 'b', reached via 'ab' and 'bc'.
During any build (incuding a zero-rebuild) we should require 'b' more than once.

  $ jenga build -a --debug-demand
  B: Require: example/a
  B: Require: example/b
  B: Require: example/c
  B: Require: example/ab
  B: Require: example/bc
  B: Require: example/top
  checked 6 rules
