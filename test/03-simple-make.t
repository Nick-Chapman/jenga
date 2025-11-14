
  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../src/jenga.exe jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/03-simple-make example

  $ jenga build -m -a
  A: grep -v '^$' defs.h.in > defs.h
  A: gcc -c fib.c -o fib.o
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 4 targets
  ran 4 commands

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/main.exe
  ,jenga/example/defs.h
  ,jenga/example/main.o

  $ ,jenga/example/main.exe
  hello, 55 world with explicit make-style rules

