
  $ here=$PWD
  $ (cd $TESTDIR/../../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/04 example

copied from the markdown...

$ jenga --help
$ jenga build -a --help

  $ jenga build -a
  A: gcc -Wall -c main.c -o main.o
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc main.o fib.o -o hello.exe
  checked 3 targets
  ran 3 commands

$ jenga build -a -f

List targets

  $ jenga build -a --list-targets
  example/hello.exe
  example/main.o
  example/fib.o

List rules

  $ jenga build -a --list-rules
  example/hello.exe : example/main.o example/fib.o
    gcc main.o fib.o -o hello.exe
  
  example/main.o : example/main.c example/fib.h
    gcc -Wall -c main.c -o main.o
  
  example/fib.o : example/fib.c example/fib.h
    gcc -Wall -c fib.c -o fib.o


The following stuff is for section 04...

Double build

  $ cp -rp example copied
  $ jenga build -a
  checked 6 targets

What are the targets?

  $ jenga build -a --list-targets
  example/hello.exe
  example/main.o
  example/fib.o
  copied/hello.exe
  copied/main.o
  copied/fib.o

Controlling the scope of what to build

  $ jenga build -a copied
  checked 3 targets

  $ jenga build -a --list-targets copied
  copied/hello.exe
  copied/main.o
  copied/fib.o
