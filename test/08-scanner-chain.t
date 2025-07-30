
  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../,jenga/src/jenga jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rp $TESTDIR/../examples/08-scanner-chain example

Initial build
  $ jenga build -a
  elaborated 6 rules and 6 targets
  A: gcc -MG -MM fib.c > fib.d
  A: gcc -c fib.c -o fib.o
  A: (echo -n 'main.d '; gcc -MG -MM main.c) > main.d2
  A: gcc -MG -MM main.c > main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 6 commands

Run the executable
  $ ,jenga/example/main.exe
  hello, 55 world with #include chain

Inspect the generated deps
  $ find ,jenga -name '*.d*' | xargs grep .
  ,jenga/example/main.d:main.o: main.c fib.h defs.h defs2.h
  ,jenga/example/main.d2:main.d main.o: main.c fib.h defs.h
  ,jenga/example/fib.d:fib.o: fib.c fib.h

Mod-A (change const value)
  $ echo '#define MY_CONST 11' > example/defs2.h
  $ jenga build -a
  elaborated 6 rules and 6 targets
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 2 commands
  $ ,jenga/example/main.exe
  hello, 89 world with #include chain
  $ find ,jenga -name '*.d*' | xargs grep .
  ,jenga/example/main.d:main.o: main.c fib.h defs.h defs2.h
  ,jenga/example/main.d2:main.d main.o: main.c fib.h defs.h
  ,jenga/example/fib.d:fib.o: fib.c fib.h

Mod-B (shorten the chain)
  $ echo '#define MY_CONST 12' > example/defs.h
  $ jenga build -a
  elaborated 6 rules and 6 targets
  A: gcc -MG -MM main.c > main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 3 commands
  $ ,jenga/example/main.exe
  hello, 144 world with #include chain
  $ find ,jenga -name '*.d*' | xargs grep .
  ,jenga/example/main.d:main.o: main.c fib.h defs.h
  ,jenga/example/main.d2:main.d main.o: main.c fib.h defs.h
  ,jenga/example/fib.d:fib.o: fib.c fib.h

Mod-C (repoint the chain)
  $ echo '#define MY_CONST 13' > example/defs3.h
  $ echo '#include "defs3.h"' > example/defs.h
  $ jenga build -a
  elaborated 6 rules and 6 targets
  A: gcc -MG -MM main.c > main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 3 commands
  $ ,jenga/example/main.exe
  hello, 233 world with #include chain
  $ find ,jenga -name '*.d*' | xargs grep .
  ,jenga/example/main.d:main.o: main.c fib.h defs.h defs3.h
  ,jenga/example/main.d2:main.d main.o: main.c fib.h defs.h
  ,jenga/example/fib.d:fib.o: fib.c fib.h
