
Get me a jenga executable and make a script to run it with a local cache

  $ (cd $TESTDIR/..; jenga build src -q) && ln $TESTDIR/../,jenga/src/jenga jenga.exe
  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH

Get me the source code for the first example...

  $ cp -rpL $TESTDIR/../examples/02-discover-deps example

Build from clean:
  $ jenga build -a
  A: cat all.files | grep '.c$' > c.files
  A: cat c.files | sed 's|\(.*\).c$|\1.d : \1.c : gcc -MG -MM \1.c -MF \1.d|' > d.rules
  A: echo gcc $(test -f cflags && cat cflags) > gcc.runner
  A: cat c.files | sed "s|\(.*\).c$|\1.o : @\1.d : $(cat gcc.runner) -c \1.c -o \1.o|" > o.rules
  A: cat c.files | sed 's|\(.*\).c|\1.o|' > o.files
  A: echo main.exe : @o.files : gcc $(cat o.files) -o main.exe > link.rule
  A: gcc -MG -MM fib.c -MF fib.d
  A: gcc -c fib.c -o fib.o
  A: gcc -MG -MM main.c -MF main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 11 targets
  ran 11 commands
  $ ,jenga/example/main.exe
  hello, 55 world with auto discovery

Zero rebuild:
  $ jenga build -a
  checked 11 targets
  $ ,jenga/example/main.exe
  hello, 55 world with auto discovery

Change main.c
  $ sed -i 's/world/UNIVERSE/g' example/main.c
  $ jenga build -a
  A: gcc -MG -MM main.c -MF main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 11 targets
  ran 3 commands
  $ ,jenga/example/main.exe
  hello, 55 UNIVERSE with auto discovery

Whitespace change to fib.h
  $ sed -i 's/int fib/int      fib/g' example/fib.h
  $ jenga build -a
  A: gcc -c fib.c -o fib.o
  A: gcc -c main.c -o main.o
  checked 11 targets
  ran 2 commands
  $ ,jenga/example/main.exe
  hello, 55 UNIVERSE with auto discovery

Change const value in defs.h
  $ echo '#define MY_CONST 11' > example/defs.h
  $ jenga build -a
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 11 targets
  ran 2 commands
  $ ,jenga/example/main.exe
  hello, 89 UNIVERSE with auto discovery

Setup ALT defs file (causes no actions):
  $ echo '#define MY_CONST 12' > example/defsALT.h
  $ jenga build -a
  A: cat all.files | grep '.c$' > c.files
  checked 11 targets
  ran 1 command

Switch main to use ALT defs:
  $ sed -i 's/defs/defsALT/g' example/main.c
  $ jenga build -a
  A: gcc -MG -MM main.c -MF main.d
  A: gcc -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 11 targets
  ran 3 commands
  $ ,jenga/example/main.exe
  hello, 144 UNIVERSE with auto discovery

Modify original defs file back to original value (causes no action):
  $ echo '#define MY_CONST 10' > example/defs.h
  $ jenga build -a
  checked 11 targets
  $ ,jenga/example/main.exe
  hello, 144 UNIVERSE with auto discovery

Switch main back to origianl defs file (causes no action)::
  $ sed -i 's/defsALT/defs/g' example/main.c
  $ jenga build -a
  checked 11 targets
  $ ,jenga/example/main.exe
  hello, 55 UNIVERSE with auto discovery


Use feature of CC setup macro which is conditionally dependent on cflags key...

Compile with -Wall:
  $ echo '-Wall' > example/cflags
  $ jenga build -a
  A: cat all.files | grep '.c$' > c.files
  A: echo gcc $(test -f cflags && cat cflags) > gcc.runner
  A: cat c.files | sed "s|\(.*\).c$|\1.o : @\1.d : $(cat gcc.runner) -c \1.c -o \1.o|" > o.rules
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  checked 11 targets
  ran 5 commands

Compile with -O2 causes relink:
  $ echo '-O2' > example/cflags
  $ jenga build -a
  A: echo gcc $(test -f cflags && cat cflags) > gcc.runner
  A: cat c.files | sed "s|\(.*\).c$|\1.o : @\1.d : $(cat gcc.runner) -c \1.c -o \1.o|" > o.rules
  A: gcc -O2 -c fib.c -o fib.o
  A: gcc -O2 -c main.c -o main.o
  A: gcc fib.o main.o -o main.exe
  checked 11 targets
  ran 5 commands
