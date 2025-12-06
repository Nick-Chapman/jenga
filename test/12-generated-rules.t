
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
  A: cat all.files | grep '.h$' > h.files
  A: gcc -MG -MM $(cat c.files) > depends
  A: gcc -c -o main.o main.c
  A: gcc -c -o fib.o fib.c
  A: cat c.files | sed 's|\(.*\).c|\1.o|' > o.files
  A: gcc -o hello.exe $(cat o.files)
  checked 9 targets
  ran 9 commands
  Hello, 55 jenga. Discovered deps and generated rules.

Change & rebuild:

  $ sed -i 's/10/11/' example/defs.h
  $ jenga build -a
  A: gcc -MG -MM $(cat c.files) > depends
  A: gcc -c -o main.o main.c
  A: gcc -o hello.exe $(cat o.files)
  checked 9 targets
  ran 3 commands
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
  example/main.o
  example/fib.o
  example/c.rules
  example/depends
  example/h.files
  example/o.files
  example/c.files
  example/hello.exe
  example/all.files

Rules:

  $ jenga build -a --list-rules
  example/main.o : example/main.c example/fib.h example/defs.h
    gcc -c -o main.o main.c
  
  example/fib.o : example/fib.c example/fib.h
    gcc -c -o fib.o fib.c
  
  example/c.rules : example/c.files
    cat c.files | sed 's|\(.*\).c$|\1.o : @depends : gcc -c -o \1.o \1.c|' > c.rules
  
  example/depends : example/c.files example/fib.c example/main.c example/defs.h example/fib.h
    gcc -MG -MM $(cat c.files) > depends
  
  example/h.files : example/all.files
    cat all.files | grep '.h$' > h.files
  
  example/o.files : example/c.files
    cat c.files | sed 's|\(.*\).c|\1.o|' > o.files
  
  example/c.files : example/all.files
    cat all.files | grep '.c$' > c.files
  
  example/hello.exe : example/o.files example/fib.o example/main.o
    gcc -o hello.exe $(cat o.files)
  
  example/all.files : 
    echo 'build.jenga\ndefs.h\nfib.c\nfib.h\nmain.c' > all.files
