
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/11-optional-dep example

Build:

  $ jenga build -a
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  A: gcc -o main.exe main.o
  ran 2 commands
  checked 2 rules

Zero build:

  $ jenga build -a
  checked 2 rules

Define CFLAGS; rebuilds:

  $ echo '-O2' > example/CFLAGS
  $ jenga build -a
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  A: gcc -o main.exe main.o
  ran 2 commands
  checked 2 rules

Change CFLAGS; rebuilds:

  $ echo '-Wall' > example/CFLAGS
  $ jenga build -a
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  (directory) example
  (rule) example/main.o : example/main.c example/CFLAGS
  (command) $ gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  (stderr) main.c:2:6: warning: return type of 'main' is not 'int' [-Wmain]
  (stderr)     2 | void main() { //main ought to be declared as int. -Wall will detect this.
  (stderr)       |      ^~~~
  ran 1 command
  checked 2 rules

Remove CFLAGS; reuse original build:

  $ rm example/CFLAGS
  $ jenga build -a
  checked 2 rules
