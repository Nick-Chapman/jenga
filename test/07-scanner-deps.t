
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/07-scanner-deps example

Initial build
  $ jenga build -a
  A: gcc -MG -MM main.c > main.d
  A: gcc -c main.c -o main.o
  A: gcc -MG -MM fib.c > fib.d
  A: gcc -c fib.c -o fib.o
  A: gcc fib.o main.o -o main.exe
  ran 5 commands
  checked 5 rules

Run the executable
  $ jenga exec example/main.exe
  hello, 144 world with scanner deps

Inspect the generated deps
  $ jenga build -mq && find ,jenga -name '*.d' | xargs cat
  main.o: main.c fib.h defs.h
  fib.o: fib.c fib.h

  $ echo '#define MY_CONST 11' > example/defs.h
  $ jenga build -a
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  ran 2 commands
  checked 5 rules
  $ jenga exec example/main.exe
  hello, 89 world with scanner deps
