
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/11-optional-dep example

Build:

  $ jenga build -a
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  A: gcc -o main.exe main.o
  checked 2 targets
  ran 2 commands

Zero build:

  $ jenga build -a
  checked 2 targets

Define CFLAGS; rebuilds:

  $ echo '-O2' > example/CFLAGS
  $ jenga build -a
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  A: gcc -o main.exe main.o
  checked 2 targets
  ran 2 commands

Change CFLAGS; rebuilds:

  $ echo '-Wall' > example/CFLAGS
  $ jenga build -a
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  (directory) example
  (rule) example/main.o : example/main.c example/CFLAGS
  (command) $ gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  (stderr) main.c:2:6: warning: return type of 'main' is not 'int' [-Wmain]
  (stderr)     2 | void main() { //m ain ought to be declared as int. -Wall will detect this.
  (stderr)       |      ^~~~
  checked 2 targets
  ran 1 command

Remove CFLAGS; reuse original build:

  $ rm example/CFLAGS
  $ jenga build -a
  checked 2 targets
