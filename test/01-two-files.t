
Get me a jenga executable and make a script to run it with a local cache

  $ (cd $TESTDIR/..; jenga build -q) && ln $TESTDIR/../src/jenga.exe jenga.exe
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

  $ jenga build -m -a
  A: gcc -c fib.c -o fib.o
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 3 targets
  ran 3 commands

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
  checked 3 targets

Update main.c "world->UNIVERSE" and rerun:

  $ sed -i 's/world/UNIVERSE/g' example/main.c
  $ jenga build -m -a
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 3 targets
  ran 2 commands
  $ ,jenga/example/main.exe
  hello, 55 UNIVERSE

Reverting to previous state of main.c causes no rebuilding:

  $ sed -i 's/UNIVERSE/world/g' example/main.c
  $ jenga build -m -a
  checked 3 targets
  $ ,jenga/example/main.exe
  hello, 55 world

Whitespace only change to main.c cause no link step (early cutoff):

  $ sed -i 's/int main/int      main/g' example/main.c
  $ jenga build -a
  A: gcc -c main.c -o main.o
  checked 3 targets
  ran 1 command

Update build rules to link executable under a different name:

  $ sed -i 's/main.exe/RENAMED.exe/' example/build.jenga
  $ jenga build -m -a
  A: gcc fib.o main.o -o RENAMED.exe
  checked 3 targets
  ran 1 command

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
  $ jenga build -m -a
  checked 3 targets

  $ find ,jenga
  ,jenga
  ,jenga/RELOCATED
  ,jenga/RELOCATED/fib.o
  ,jenga/RELOCATED/RENAMED.exe
  ,jenga/RELOCATED/main.o

Duplicate the example directory; double elaborated rules; still no rebuilds:

  $ cp -rp RELOCATED ANOTHER
  $ jenga build -m -a
  checked 6 targets

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
  $ jenga build -m -a
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o RENAMED.exe
  checked 6 targets
  ran 2 commands

Run the two versions:

  $ ,jenga/RELOCATED/RENAMED.exe
  hello, 6765 world
  $ ,jenga/ANOTHER/RENAMED.exe
  hello, 55 world

Materalize all targets:

  $ jenga build -m -a
  checked 6 targets

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

  $ jenga build -m -a
  checked 6 targets

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
  $ jenga build -m -a
  checked 3 targets
  $ find ,jenga
  ,jenga
  ,jenga/ANOTHER
  ,jenga/ANOTHER/fib.o
  ,jenga/ANOTHER/RENAMED.exe
  ,jenga/ANOTHER/main.o

Mod some more, try -q

  $ sed -i 's/fib(10)/fib(11)/g' ANOTHER/main.c
  $ jenga build -m -q
  $ ,jenga/ANOTHER/RENAMED.exe
  hello, 89 world

Mod again, use "jenga exec"

  $ sed -i 's/fib(11)/fib(12)/g' ANOTHER/main.c
  $ jenga exec -m ANOTHER/RENAMED.exe
  hello, 144 world
