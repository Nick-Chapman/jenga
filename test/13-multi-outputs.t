
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

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
  ran 8 commands
  checked 3 rules

And zero
  $ jenga build -a
  checked 3 rules

Change the example to echo a warning before exiting

  $ sed -i 's|exit 0|echo WARNING; exit 0|' example/build.jenga
  $ jenga build -a
  A: head -1 lots > h
  A: tail -1 lots > t
  A: touch x
  A: echo WARNING; exit 0
  (directory) example
  (rule) example/h example/t : example/lots
  (command) $ head -1 lots > h
  (command) $ tail -1 lots > t
  (command) $ touch x
  (command) $ echo WARNING; exit 0
  WARNING
  ran 4 commands
  checked 3 rules

And zero build (see warning even though no actions were run)

  $ jenga build -a
  (directory) example
  (rule) example/h example/t : example/lots
  (command) $ head -1 lots > h
  (command) $ tail -1 lots > t
  (command) $ touch x
  (command) $ echo WARNING; exit 0
  WARNING
  checked 3 rules

Change the example to have a non-zero error code

  $ sed -i 's|exit 0|exit 42|' example/build.jenga
  $ jenga build -a
  A: head -1 lots > h
  A: tail -1 lots > t
  A: touch x
  A: echo WARNING; exit 42
  (directory) example
  (rule) example/h example/t : example/lots
  (command) $ head -1 lots > h
  (command) $ tail -1 lots > t
  (command) $ touch x
  (command) $ echo WARNING; exit 42
  WARNING
  (exit-code) 42
  ran 4 commands
  Build failed for 1 reasons:
  (1) action failed for rule targeting: example/h example/t

And zero build (see warning and error, again even though no actions were run)

  $ jenga build -a
  (directory) example
  (rule) example/h example/t : example/lots
  (command) $ head -1 lots > h
  (command) $ tail -1 lots > t
  (command) $ touch x
  (command) $ echo WARNING; exit 42
  WARNING
  (exit-code) 42
  Build failed for 1 reasons:
  (1) action failed for rule targeting: example/h example/t
