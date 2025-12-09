
  $ here=$PWD
  $ (cd $TESTDIR/../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

  $ cp -rp $TESTDIR/../examples/12-generated-rules example

Build:

  $ jenga build -a && jenga exec example/hello.exe
  A: echo 'build.jenga\ndefs.h\nfib.c\nfib.h\nmain.c' > all.files
  A: cat all.files | grep '.c$' > c.files
  A: cat c.files | sed 's|\(.*\).c$|\1.o : @depends : gcc -c -o \1.o \1.c|' > c.rules
  A: cat c.files | sed 's|\(.*\).c|\1.o|' > o.files
  A: cat all.files | grep '.h$' > h.files
  A: gcc -MG -MM $(cat c.files) > depends
  A: gcc -c -o fib.o fib.c
  A: gcc -c -o main.o main.c
  A: gcc -o hello.exe $(cat o.files)
  ran 9 commands
  checked 9 rules
  Hello, 55 jenga. Discovered deps and generated rules.

Change & rebuild:

  $ sed -i 's/10/11/' example/defs.h
  $ jenga build -a
  A: gcc -MG -MM $(cat c.files) > depends
  A: gcc -c -o main.o main.c
  A: gcc -o hello.exe $(cat o.files)
  ran 3 commands
  checked 9 rules
  $ jenga exec example/hello.exe
  Hello, 89 jenga. Discovered deps and generated rules.

Artifacts (materialize all)

  $ jenga build -mq && find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/c.rules
  ,jenga/example/c.files
  ,jenga/example/depends
  ,jenga/example/hello.exe
  ,jenga/example/o.files
  ,jenga/example/h.files
  ,jenga/example/all.files
  ,jenga/example/main.o

Targets:

  $ jenga build -a --list-targets
  example/all.files
  example/hello.exe
  example/c.files
  example/o.files
  example/h.files
  example/depends
  example/c.rules
  example/fib.o
  example/main.o

Rules:

  $ jenga build -a --list-rules
  example/all.files : 
    echo 'build.jenga\ndefs.h\nfib.c\nfib.h\nmain.c' > all.files
  
  example/hello.exe : example/o.files example/fib.o example/main.o
    gcc -o hello.exe $(cat o.files)
  
  example/c.files : example/all.files
    cat all.files | grep '.c$' > c.files
  
  example/o.files : example/c.files
    cat c.files | sed 's|\(.*\).c|\1.o|' > o.files
  
  example/h.files : example/all.files
    cat all.files | grep '.h$' > h.files
  
  example/depends : example/c.files example/fib.c example/main.c example/defs.h example/fib.h
    gcc -MG -MM $(cat c.files) > depends
  
  example/c.rules : example/c.files
    cat c.files | sed 's|\(.*\).c$|\1.o : @depends : gcc -c -o \1.o \1.c|' > c.rules
  
  example/fib.o : example/fib.c example/fib.h
    gcc -c -o fib.o fib.c
  
  example/main.o : example/main.c example/fib.h example/defs.h
    gcc -c -o main.o main.c
