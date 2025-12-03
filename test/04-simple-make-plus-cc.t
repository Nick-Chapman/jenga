
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rpL $TESTDIR/../examples/04-simple-make-plus-cc example

  $ jenga build -a
  A: echo 'defs.h.in\nfib.c\nmain.c\nfib.h\ncc.jenga\nREADME\nbuild.jenga' | grep '\.c$' > c.files
  A: cat c.files | sed 's|\(.*\).c$|\1.d : \1.c : gcc -MG -MM \1.c -MF \1.d|' > d.rules
  A: echo gcc $(test -f cflags && cat cflags) > gcc.runner
  A: cat c.files | sed "s|\(.*\).c$|\1.o : @\1.d : $(cat gcc.runner) -c \1.c -o \1.o|" > o.rules
  A: cat c.files | sed 's|\(.*\).c|\1.o|' > o.files
  A: echo main.exe : @o.files : gcc $(cat o.files) -o main.exe > link.rule
  A: grep -v '^$' defs.h.in > defs.h
  A: gcc -MG -MM fib.c -MF fib.d
  A: gcc -c fib.c -o fib.o
  A: gcc -MG -MM main.c -MF main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 12 targets
  ran 12 commands

  $ jenga exec example/main.exe
  hello, 55 world with combined cc and make configs

