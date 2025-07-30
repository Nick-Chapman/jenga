
Get me a jenga executable and make a script to run it with a local cache

  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../,jenga/src/jenga jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH

Get me the source code for the first example...

  $ cp -rp $TESTDIR/../examples/01-two-files example

What have I got?

  $ find .
  .
  ./jenga.exe
  ./example
  ./example/fib.c
  ./example/main.c
  ./example/build.jenga
  ./jenga

Build from clean:

  $ jenga build -a
  elaborated 3 rules and 3 targets
  A: gcc -c fib.c -o fib.o
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 3 actions

See the artifacts:

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/main.exe
  ,jenga/example/main.o

Run the built artifact:

  $ ,jenga/example/main.exe
  hello, 55 world

Rebuild after no changes:

  $ jenga build -a
  elaborated 3 rules and 3 targets

Update main.c "world->UNIVERSE" and rerun:

  $ sed -i 's/world/UNIVERSE/g' example/main.c
  $ jenga build -a
  elaborated 3 rules and 3 targets
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/example/main.exe
  hello, 55 UNIVERSE

Reverting to previous state of main.c causes no rebuilding:

  $ sed -i 's/UNIVERSE/world/g' example/main.c
  $ jenga build -a
  elaborated 3 rules and 3 targets
  $ ,jenga/example/main.exe
  hello, 55 world

Whitespace only change to main.c cause no link step (early cutoff):

  $ sed -i 's/int main/int      main/g' example/main.c
  $ jenga build -a
  elaborated 3 rules and 3 targets
  A: gcc -c main.c -o main.o
  ran 1 action

Update build rules to link executable under a different name:

  $ sed -i 's/main.exe/RENAMED.exe/' example/build.jenga
  $ jenga build -a
  elaborated 3 rules and 3 targets
  A: gcc fib.o main.o -o RENAMED.exe
  ran 1 action

  $ ,jenga/example/RENAMED.exe
  hello, 55 world

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/RENAMED.exe
  ,jenga/example/main.o

Relocate the example to a new directory; no rebuilds:

  $ mv example RELOCATED
  $ jenga build -a
  elaborated 3 rules and 3 targets

  $ find ,jenga
  ,jenga
  ,jenga/RELOCATED
  ,jenga/RELOCATED/fib.o
  ,jenga/RELOCATED/RENAMED.exe
  ,jenga/RELOCATED/main.o

Duplicate the example directory; double elaborated rules; still no rebuilds:

  $ cp -rp RELOCATED ANOTHER
  $ jenga build -a
  elaborated 6 rules and 6 targets

  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/fib.o
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/ANOTHER/main.o
  ,jenga/RELOCATED
  ,jenga/RELOCATED/fib.o
  ,jenga/RELOCATED/RENAMED.exe
  ,jenga/RELOCATED/main.o

Modify code in one of the example directories; minimal rebuild as required:

  $ sed -i 's/fib(10)/fib(20)/g' RELOCATED/main.c
  $ jenga build -a
  elaborated 6 rules and 6 targets
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o RENAMED.exe
  ran 2 actions

Run the two versions:

  $ ,jenga/RELOCATED/RENAMED.exe
  hello, 6765 world
  $ ,jenga/ANOTHER/RENAMED.exe
  hello, 55 world

Materalize all targets:

  $ jenga build -a
  elaborated 6 rules and 6 targets

  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/fib.o
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/ANOTHER/main.o
  ,jenga/RELOCATED
  ,jenga/RELOCATED/fib.o
  ,jenga/RELOCATED/RENAMED.exe
  ,jenga/RELOCATED/main.o

Materalize just artifacts:

  $ jenga build -a
  elaborated 6 rules and 6 targets

  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/fib.o
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/ANOTHER/main.o
  ,jenga/RELOCATED
  ,jenga/RELOCATED/fib.o
  ,jenga/RELOCATED/RENAMED.exe
  ,jenga/RELOCATED/main.o

Remove one directory copy

  $ rm -rf RELOCATED
  $ jenga build -a
  elaborated 3 rules and 3 targets
  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/fib.o
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/ANOTHER/main.o

Mod some more, try -q

  $ sed -i 's/fib(10)/fib(11)/g' ANOTHER/main.c
  $ jenga build -q
  $ ,jenga/ANOTHER/RENAMED.exe
  hello, 89 world

Mod again, use "jenga run"

  $ sed -i 's/fib(11)/fib(12)/g' ANOTHER/main.c
  $ jenga run ANOTHER/RENAMED.exe
  hello, 144 world
