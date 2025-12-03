
  $ here=$PWD
  $ (cd $TESTDIR/../../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

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

  $ jenga exec hello.exe
  Hello, jenga world!

Modify file and rebuild. See the rebuild action, and the changed output.

  $ sed -i s/world/universe/ main.c
  $ jenga build -a && jenga exec hello.exe
  A: gcc main.c -o hello.exe
  checked 1 target
  ran 1 command
  Hello, jenga universe!

Reset file. See no actions (they were cached), but the output reverted.

  $ sed -i s/universe/world/ main.c
  $ jenga build -a && jenga exec hello.exe
  checked 1 target
  Hello, jenga world!
