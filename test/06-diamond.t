
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/06-diamond example

  $ jenga build -a
  A: echo -n A > a
  A: echo -n B  > b
  A: cat a b > ab
  A: echo -n C > c
  A: cat b c > bc
  A: cat ab bc > top
  checked 6 targets
  ran 6 commands

  $ jenga build -mq && cat ,jenga/example/top
  ABBC (no-eol)

The example has a diamond dependency on target 'b', reached via 'ab' and 'bc'.
During any build (incuding a zero-rebuild) we should require 'b' more than once.

  $ jenga build -a --debug-demand
  B: Require: example/top
  B: Require: example/ab
  B: Require: example/a
  B: Require: example/b
  B: Require: example/bc
  B: Require: example/c
  checked 6 targets
