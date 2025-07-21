
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

  $ jenga build
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

  $ jenga build
  elaborated 3 rules and 3 targets

Add '-x' flag for a more detailed logging.

  $ jenga build -x
  X: md5sum example/build.jenga
  elaborated 3 rules and 3 targets
  X: md5sum example/fib.c
  X: md5sum example/main.c

Add '-xi flags for very detailed logging. Test will be fragile to any change in example source.
We take care to mask any pids referenced by sanboxes

  $ jenga build -xi | sed 's|jbox/[0-9]*|jbox/$$|'
  I: rm -rf ,jenga
  I: mkdir -p .cache/jenga/files
  I: mkdir -p .cache/jenga/traces
  I: mkdir -p ,jenga
  I: test -e .
  I: test -d .
  I: ls .
  I: test -d jenga.exe
  I: test -d .cache
  I: test -d example
  I: test -d jenga
  I: test -d ,jenga
  I: ls example
  I: test -d example/fib.c
  I: test -d example/main.c
  I: test -d example/build.jenga
  I: test -e build.jenga
  I: test -e example/build.jenga
  I: ls example
  I: test -e example/all.files
  I: test -e example/build.jenga
  X: md5sum example/build.jenga
  I: test -e .cache/jenga/files/3af6909c0c5bef3721f03645025ca17c
  I: cat .cache/jenga/files/3af6909c0c5bef3721f03645025ca17c
  I: test -e example/main.o
  I: test -e example/fib.o
  I: test -e example/main.exe
  elaborated 3 rules and 3 targets
  I: test -e example/fib.c
  X: md5sum example/fib.c
  I: test -e .cache/jenga/files/3ec221831446382d711ea3ce24237158
  I: test -e .cache/jenga/traces/b18ebe03eeaef908caa477b54bdfb363
  I: cat .cache/jenga/traces/b18ebe03eeaef908caa477b54bdfb363
  I: test -e .cache/jenga/files/47a0ee09b975f7501dbeb5431b76c24c
  I: test -e example/main.c
  X: md5sum example/main.c
  I: test -e .cache/jenga/files/9d125f57501617a7e09da68a33e65d1c
  I: test -e .cache/jenga/traces/5d6d768de8b220a93652d885cd07d942
  I: cat .cache/jenga/traces/5d6d768de8b220a93652d885cd07d942
  I: test -e .cache/jenga/files/aac22b6d9cbb6711115a1ebde2cfd6a1
  I: test -e .cache/jenga/traces/e4071a5b79f05a91d7209f979dca43c2
  I: cat .cache/jenga/traces/e4071a5b79f05a91d7209f979dca43c2
  I: test -e .cache/jenga/files/9efc05831ccef0c24b2697d8fff2acee
  I: mkdir -p ,jenga/example
  I: ln .cache/jenga/files/9efc05831ccef0c24b2697d8fff2acee ,jenga/example/main.exe
  I: mkdir -p ,jenga/example
  I: ln .cache/jenga/files/47a0ee09b975f7501dbeb5431b76c24c ,jenga/example/fib.o
  I: mkdir -p ,jenga/example
  I: ln .cache/jenga/files/aac22b6d9cbb6711115a1ebde2cfd6a1 ,jenga/example/main.o
  I: rm -rf /tmp/.jbox/$$

Update main.c "world->UNIVERSE" and rerun:

  $ sed -i 's/world/UNIVERSE/g' example/main.c
  $ jenga build
  elaborated 3 rules and 3 targets
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/example/main.exe
  hello, 55 UNIVERSE

Reverting to previous state of main.c causes no rebuilding:

  $ sed -i 's/UNIVERSE/world/g' example/main.c
  $ jenga build
  elaborated 3 rules and 3 targets
  $ ,jenga/example/main.exe
  hello, 55 world

Whitespace only change to main.c cause no link step (early cutoff):

  $ sed -i 's/int main/int      main/g' example/main.c
  $ jenga build
  elaborated 3 rules and 3 targets
  A: gcc -c main.c -o main.o
  ran 1 action

Update build rules to link executable under a different name:

  $ sed -i 's/main.exe/RENAMED.exe/' example/build.jenga
  $ jenga build
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
  $ jenga build
  elaborated 3 rules and 3 targets

  $ find ,jenga
  ,jenga
  ,jenga/RELOCATED
  ,jenga/RELOCATED/fib.o
  ,jenga/RELOCATED/RENAMED.exe
  ,jenga/RELOCATED/main.o

Duplicate the example directory; double elaborated rules; still no rebuilds:

  $ cp -rp RELOCATED ANOTHER
  $ jenga build
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
  $ jenga build
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

  $ jenga build
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

  $ jenga build
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
  $ jenga build
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
