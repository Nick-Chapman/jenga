
  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../src/jenga.exe jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH
  $ cp -rpL $TESTDIR/../examples/04-simple-make-plus-cc example

  $ jenga build -m -a
  A: cat all.files | grep '.c$' > c.files
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

  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/main.exe
  ,jenga/example/main.d
  ,jenga/example/c.files
  ,jenga/example/defs.h
  ,jenga/example/o.files
  ,jenga/example/o.rules
  ,jenga/example/gcc.runner
  ,jenga/example/fib.d
  ,jenga/example/link.rule
  ,jenga/example/d.rules
  ,jenga/example/main.o

  $ ,jenga/example/main.exe
  hello, 55 world with combined cc and make configs

