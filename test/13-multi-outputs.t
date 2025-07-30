
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
  A: exit 0
  A: cat h t > final
  checked 4 targets
  ran 8 commands

And zero
  $ jenga build -a
  checked 4 targets

Change the example to echo a warning before exiting

  $ sed -i 's|exit 0|echo WARNING; exit 0|' example/build.jenga
  $ jenga build -a
  A: head -1 lots > h
  A: tail -1 lots > t
  A: touch x
  A: echo WARNING; exit 0
  WARNING
  checked 4 targets
  ran 4 commands

And zero build (see warning even though no actions were run)

  $ jenga build -a
  WARNING
  checked 4 targets

Change the example to have a non-zero error code

  $ sed -i 's|exit 0|exit 42|' example/build.jenga
  $ jenga build -a
  A: head -1 lots > h
  A: tail -1 lots > t
  A: touch x
  A: echo WARNING; exit 42
  WARNING
  ExitFailure 42
  ran 4 commands
  Build failed for 1 reasons:
  (1) 'example/h example/t': action failed for rule 'example/build.jenga:7'

And zero build (see warning and error, again even though no actions were run)

  $ jenga build -a
  WARNING
  ExitFailure 42
  Build failed for 1 reasons:
  (1) 'example/h example/t': action failed for rule 'example/build.jenga:7'
