
  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../,jenga/src/jenga jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/11-optional-dep example

Build:

  $ jenga build -a
  elaborated 2 rules and 2 targets
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  A: gcc -o main.exe main.o
  ran 2 commands

Zero build:

  $ jenga build -a
  elaborated 2 rules and 2 targets

Define CFLAGS; rebuilds:

  $ echo '-O2' > example/CFLAGS
  $ jenga build -a
  elaborated 2 rules and 2 targets
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  A: gcc -o main.exe main.o
  ran 2 commands

Change CFLAGS; rebuilds:

  $ echo '-Wall' > example/CFLAGS
  $ jenga build -a
  elaborated 2 rules and 2 targets
  A: gcc -c $(test -f CFLAGS && cat CFLAGS) main.c
  main.c:2:6: warning: return type of 'main' is not 'int' [-Wmain]
      2 | void main() { //m ain ought to be declared as int. -Wall will detect this.
        |      ^~~~
  ran 1 command

Remove CFLAGS; reuse original build:

  $ rm example/CFLAGS
  $ jenga build -a
  elaborated 2 rules and 2 targets
