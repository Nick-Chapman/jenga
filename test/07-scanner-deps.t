
  $ ln $(find $TESTDIR/../.stack-work/dist -type f -name main.exe) jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/07-scanner-deps example

Initial build
  $ jenga build
  elaborated 5 rules and 5 targets
  A: gcc -MG -MM fib.c > fib.d
  A: gcc -c fib.c -o fib.o
  A: gcc -MG -MM main.c > main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 5 actions

Run the executable
  $ ,jenga/example/main.exe
  hello, 55 world with scanner deps

Inspect the generated deps
  $ find ,jenga -name '*.d' | xargs cat
  main.o: main.c fib.h defs.h
  fib.o: fib.c fib.h

  $ echo '#define MY_CONST 11' > example/defs.h
  $ jenga build
  elaborated 5 rules and 5 targets
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 2 actions
  $ ,jenga/example/main.exe
  hello, 89 world with scanner deps
