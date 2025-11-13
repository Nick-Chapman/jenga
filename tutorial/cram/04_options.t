

  $ (cd $TESTDIR/../..; jenga build -m src -q)
  $ echo exec $TESTDIR/../../,jenga/src/jenga '"$@"' > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/04 example

copied from the markdown...

$ jenga --help
$ jenga build -m -a --help

  $ jenga build -m -a -c.
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 3 targets
  ran 3 commands

$ jenga build -m -a -f

List targets

  $ jenga build -m -a --list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe

List rules

  $ jenga build -m -a --list-rules -c.
  example/fib.o : example/fib.c example/fib.h
    cd example ; gcc -Wall -c fib.c -o fib.o
  
  example/main.o : example/main.c example/fib.h
    cd example ; gcc -Wall -c main.c -o main.o
  
  example/hello.exe : example/main.o example/fib.o
    cd example ; gcc main.o fib.o -o hello.exe


The following stuff is for section 04...

Double build

  $ cp -rp example copied
  $ jenga build -m -a -c.
  checked 6 targets

What are the targets?

  $ jenga build -m -a --list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe
  copied/fib.o
  copied/main.o
  copied/hello.exe

  $ jenga build -m -a --list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe
  copied/fib.o
  copied/main.o
  copied/hello.exe

Controlling the scope of what to build

  $ jenga build -m -a copied -c.
  checked 3 targets

  $ jenga build -m -a --list-targets copied -c.
  copied/fib.o
  copied/main.o
  copied/hello.exe

Temporary cache

  $ jenga build -m -a -f | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
  using temporary cache: /tmp/.cache/jenga/$$
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 6 targets
  ran 3 commands

Non deterministic
$ jenga build -m -a -fj2 | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
$ jenga build -m -a -fj2 --show-pid
