
  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../,jenga/src/jenga jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/13-multi-outputs example

Build:
  $ jenga build -a
  A: echo 11 >> lots
  A: echo 22 >> lots
  A: echo 33 >> lots
  A: head -1 lots > h
  A: tail -1 lots > t
  A: touch x
  A: echo Here is a warning message!
  A: exit 0
  Here is a warning message!
  A: cat h t > final
  checked 4 targets
  ran 9 commands

And zero
  $ jenga build -a
  checked 4 targets

Change the example to error:
  $ sed -i 's|exit 0|exit 42|' example/build.jenga
  $ jenga build -a
  A: head -1 lots > h
  A: tail -1 lots > t
  A: touch x
  A: echo Here is a warning message!
  A: exit 42
  Here is a warning message!
  ExitFailure 42
  ran 5 commands
  Build failed for 1 reasons:
  (1) 'example/h example/t': action failed for rule 'example/build.jenga:7'

And zero
  $ jenga build -a
  Here is a warning message!
  ExitFailure 42
  Build failed for 1 reasons:
  (1) 'example/h example/t': action failed for rule 'example/build.jenga:7'
