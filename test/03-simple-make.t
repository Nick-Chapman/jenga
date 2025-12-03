
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/03-simple-make example

  $ jenga build -a
  A: grep -v '^$' defs.h.in > defs.h
  A: gcc -c fib.c -o fib.o
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 4 targets
  ran 4 commands

Run the materizalized executable

  $ jenga exec example/main.exe
  hello, 55 world with explicit make-style rules

