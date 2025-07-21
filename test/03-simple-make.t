
  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../,jenga/src/jenga jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/03-simple-make example

  $ jenga build
  elaborated 4 rules and 4 targets
  A: grep -v '^$' defs.h.in > defs.h
  A: gcc -c fib.c -o fib.o
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 4 actions

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/main.exe
  ,jenga/example/defs.h
  ,jenga/example/main.o

  $ ,jenga/example/main.exe
  hello, 55 world with explicit make-style rules

