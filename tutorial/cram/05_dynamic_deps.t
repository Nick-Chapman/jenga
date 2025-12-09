
  $ here=$PWD
  $ (cd $TESTDIR/../../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
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

  $ jenga build -a
  A: gcc -MG -MM *.c > depends
  A: gcc -Wall -c main.c -o main.o
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc main.o fib.o -o hello.exe
  ran 4 commands
  checked 4 rules

Run the executable

  $ jenga exec example/hello.exe
  Hello, 55 jenga!

See the depends

  $ jenga build -mq && cat ,jenga/example/depends
  fib.o: fib.c fib.h
  main.o: main.c fib.h

See the targets and rules

  $ jenga build -a --list-targets
  example/hello.exe
  example/main.o
  example/fib.o
  example/depends

  $ jenga build -a -r
  example/hello.exe : example/main.o example/fib.o
    gcc main.o fib.o -o hello.exe
  
  example/main.o : example/main.c example/fib.h
    gcc -Wall -c main.c -o main.o
  
  example/fib.o : example/fib.c example/fib.h
    gcc -Wall -c fib.c -o fib.o
  
  example/depends : example/main.c example/fib.c
    gcc -MG -MM *.c > depends

  $ (cd example; ../jenga build -r)
  hello.exe : main.o fib.o
    gcc main.o fib.o -o hello.exe
  
  main.o : main.c fib.h
    gcc -Wall -c main.c -o main.o
  
  fib.o : fib.c fib.h
    gcc -Wall -c fib.c -o fib.o
  
  depends : main.c fib.c
    gcc -MG -MM *.c > depends
