

  $ (cd $TESTDIR/../..; jenga build src -q)
  $ echo exec $TESTDIR/../../,jenga/src/jenga '"$@"' > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/04 example

copied from the markdown...

$ jenga --help
$ jenga build --help

  $ jenga build -c.
  elaborated 3 rules and 3 targets
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  ran 3 actions

$ jenga build -f

List targets

  $ jenga list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe

List rules

  $ jenga list-rules -c.
  example/fib.o : example/fib.c example/fib.h
    cd example ; gcc -Wall -c fib.c -o fib.o
  
  example/main.o : example/main.c example/fib.h
    cd example ; gcc -Wall -c main.c -o main.o
  
  example/hello.exe : example/main.o example/fib.o
    cd example ; gcc main.o fib.o -o hello.exe


The following stuff is for section 04...

Double build

  $ cp -rp example copied
  $ jenga build -c.
  elaborated 6 rules and 6 targets

What are the targets?

  $ jenga list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe
  copied/fib.o
  copied/main.o
  copied/hello.exe

  $ jenga list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe
  copied/fib.o
  copied/main.o
  copied/hello.exe

Controlling the scope of what to build

  $ jenga build copied -c.
  elaborated 3 rules and 3 targets

  $ jenga list-targets copied -c.
  copied/fib.o
  copied/main.o
  copied/hello.exe

Temporary cache

  $ jenga build -f | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
  using temporary cache: /tmp/.cache/jenga/$$
  elaborated 6 rules and 6 targets
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  ran 3 actions

Non deterministic
$ jenga build -fj2 | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
$ jenga build -fj2 --show-pid
