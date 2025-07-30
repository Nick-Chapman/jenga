
This cram file accompanies the jenga tutorial document.

Get a jenga executable
  $ (cd $TESTDIR/../..; jenga build src -q) && ln $TESTDIR/../../,jenga/src/jenga jenga.exe

Make a small script to run jenga with a local cache.
Avoiding interference from the global cache, which will make this test non-deterministic.

  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH

Get the example.
  $ cp -rp $TESTDIR/../files/01/build.jenga .
  $ cp -rp $TESTDIR/../files/01/main.c .

Build. See one build action.
  $ jenga build -a
  elaborated 1 rule and 1 target
  A: gcc main.c -o hello.exe
  ran 1 action

Zero rebuild. See no build actions.
  $ jenga build -a
  elaborated 1 rule and 1 target

Run built executable. See no actions, then the hello program output.
  $ jenga run hello.exe
  Hello, jenga world!

Modify file and rebuild. See the rebuild action, and the changed output.
  $ sed -i s/world/universe/ main.c
  $ jenga build -a; ,jenga/hello.exe
  elaborated 1 rule and 1 target
  A: gcc main.c -o hello.exe
  ran 1 action
  Hello, jenga universe!

Reset file. See no actions (they were cached), but the output reverted.
  $ sed -i s/universe/world/ main.c
  $ jenga build -a; ,jenga/hello.exe
  elaborated 1 rule and 1 target
  Hello, jenga world!
