
  $ (cd $TESTDIR/../..; jenga build src -q)
  $ echo exec $TESTDIR/../../src/jenga.exe '"$@"' --rel > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/04 example

copied from the markdown...

$ jenga --help
$ jenga build -a --help

  $ jenga build -a -c.
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 3 targets
  ran 3 commands

$ jenga build -a -f

List targets

  $ jenga build -a --list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe

List rules

  $ jenga build -a --list-rules -c.
  example/fib.o : example/fib.c example/fib.h
    gcc -Wall -c fib.c -o fib.o
  
  example/main.o : example/main.c example/fib.h
    gcc -Wall -c main.c -o main.o
  
  example/hello.exe : example/main.o example/fib.o
    gcc main.o fib.o -o hello.exe


The following stuff is for section 04...

Double build

  $ cp -rp example copied
  $ jenga build -a -c.
  checked 6 targets

What are the targets?

  $ jenga build -a --list-targets -c.
  copied/fib.o
  copied/main.o
  copied/hello.exe
  example/fib.o
  example/main.o
  example/hello.exe

Controlling the scope of what to build

  $ jenga build -a copied -c.
  checked 3 targets

  $ jenga build -a --list-targets copied -c.
  copied/fib.o
  copied/main.o
  copied/hello.exe

Temporary cache

  $ jenga build -a -f | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
  using temporary cache: /tmp/.cache/jenga/$$
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 6 targets
  ran 3 commands

Non deterministic
$ jenga build -a -fj2 | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
$ jenga build -a -fj2 --show-pid
