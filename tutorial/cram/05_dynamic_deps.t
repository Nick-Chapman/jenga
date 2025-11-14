
Get an up-to-date jenga executable in path, which runs with a local cachee

  $ (cd $TESTDIR/../..; jenga build -q)
  $ echo exec $TESTDIR/../../src/jenga.exe '"$@"' > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/05 example

  $ find example
  example
  example/fib.c
  example/main.c
  example/fib.h
  example/build.jenga

  $ cat example/build.jenga
  
  hello.exe : main.o fib.o
    gcc main.o fib.o -o hello.exe
  
  main.o : @depends
    gcc -Wall -c main.c -o main.o
  
  fib.o : @depends
    gcc -Wall -c fib.c -o fib.o
  
  depends : main.c fib.c
    gcc -MG -MM *.c > depends

Build. Expect 4 actions to be run

  $ jenga build -a -c.
  A: gcc -MG -MM *.c > depends
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 4 targets
  ran 4 commands

Run the executable

  $ jenga exec -m example/hello.exe -c.
  Hello, 55 jenga!

See the depends

  $ cat ,jenga/example/depends
  fib.o: fib.c fib.h
  main.o: main.c fib.h

See the targets and rules

  $ jenga build -a --list-targets -c.
  example/depends
  example/fib.o
  example/main.o
  example/hello.exe

  $ jenga build -a -r -c.
  example/depends : example/main.c example/fib.c
    cd example ; gcc -MG -MM *.c > depends
  
  example/fib.o : example/fib.c example/fib.h
    cd example ; gcc -Wall -c fib.c -o fib.o
  
  example/main.o : example/main.c example/fib.h
    cd example ; gcc -Wall -c main.c -o main.o
  
  example/hello.exe : example/main.o example/fib.o
    cd example ; gcc main.o fib.o -o hello.exe

  $ (cd example; ../jenga build -r -c..)
  depends : main.c fib.c
    gcc -MG -MM *.c > depends
  
  fib.o : fib.c fib.h
    gcc -Wall -c fib.c -o fib.o
  
  main.o : main.c fib.h
    gcc -Wall -c main.c -o main.o
  
  hello.exe : main.o fib.o
    gcc main.o fib.o -o hello.exe

  $ (cd example; ../jenga build -arf)  | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
  using temporary cache: /tmp/.cache/jenga/$$
  A: gcc -MG -MM *.c > depends
  depends : main.c fib.c
    gcc -MG -MM *.c > depends
  
  fib.o : fib.c fib.h
    gcc -Wall -c fib.c -o fib.o
  
  main.o : main.c fib.h
    gcc -Wall -c main.c -o main.o
  
  hello.exe : main.o fib.o
    gcc main.o fib.o -o hello.exe
  ran 1 command
