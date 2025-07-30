
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
  A: gcc main.c -o hello.exe
  checked 1 target
  ran 1 command

Zero rebuild. See no build actions.
  $ jenga build -a
  checked 1 target

Run built executable. See no actions, then the hello program output.
  $ jenga run hello.exe
  Hello, jenga world!

Modify file and rebuild. See the rebuild action, and the changed output.
  $ sed -i s/world/universe/ main.c
  $ jenga build -a; ,jenga/hello.exe
  A: gcc main.c -o hello.exe
  checked 1 target
  ran 1 command
  Hello, jenga universe!

Reset file. See no actions (they were cached), but the output reverted.
  $ sed -i s/universe/world/ main.c
  $ jenga build -a; ,jenga/hello.exe
  checked 1 target
  Hello, jenga world!
