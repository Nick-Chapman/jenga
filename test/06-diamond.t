
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/example-06-diamond example

  $ jenga build
  elaborated 6 rules and 6 targets
  materalizing 1 artifact
  A: echo -n A > a
  A: echo -n B  > b
  A: cat a b > ab
  A: echo -n C > c
  A: cat b c > bc
  A: cat ab bc > top
  ran 6 actions

  $ cat ,jenga/example/top
  ABBC (no-eol)

The example has a diamond dependency on target 'b', reached via 'ab' and 'bc'.
During any build (incuding a zero-rebuild) we should require 'b' more than once.

  $ jenga build -v
  elaborated 6 rules and 6 targets
  materalizing 1 artifact
  B: Require: example/top
  B: Require: example/ab
  B: Require: example/a
  B: Require: example/b
  B: Require: example/bc
  B: Require: example/c
