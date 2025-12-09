
  $ here=$PWD
  $ (cd $TESTDIR/../../src; jenga install jenga.exe $here/jenga.exe)
  $ echo exec $PWD/jenga.exe '"$@"' -j1 --rel --cache=$PWD > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/02/build.jenga .
  $ cp -rp $TESTDIR/../files/02/main.c .
  $ cp -rp $TESTDIR/../files/02/fib.c .

Initial build. Expect 3 actions to be run

  $ jenga build -a
  A: gcc -c main.c -o main.o
  A: gcc -c fib.c -o fib.o
  A: gcc main.o fib.o -o hello.exe
  checked 3 targets
  ran 3 commands

Running executable returns exit code 17

  $ jenga build -mq && ,jenga/hello.exe; echo $?
  Hello, 55 jenga!
  17

Add -Wall to both compile rule. Two actions get rerun.

  $ sed -i 's/-c/-Wall -c/' build.jenga
  $ cat build.jenga
  hello.exe : main.o fib.o
    gcc main.o fib.o -o hello.exe
  
  main.o : main.c
    gcc -Wall -c main.c -o main.o
  
  fib.o : fib.c
    gcc -Wall -c fib.c -o fib.o

  $ jenga build -a
  A: gcc -Wall -c main.c -o main.o
  (directory) .
  (rule) main.o : main.c
  (command) $ gcc -Wall -c main.c -o main.o
  (stderr) main.c:3:6: warning: return type of 'main' is not 'int' [-Wmain]
  (stderr)     3 | void main() { // Oops! main should be declared to return int.
  (stderr)       |      ^~~~
  A: gcc -Wall -c fib.c -o fib.o
  checked 3 targets
  ran 2 commands

Fix code. Compile of main.c and link are rerun

  $ sed -i 's/void main/int main/' main.c
  $ cat main.c
  #include <stdio.h>
  int fib(int);
  int main() { // Oops! main should be declared to return int.
    printf("Hello, %d jenga!\n", fib(10));
  }
  $ jenga build -a
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 3 targets
  ran 2 commands
  $ jenga exec hello.exe
  Hello, 55 jenga!

Define and use header file. Build fails because we failed to declare dependecy on fib.h
  $ cp -rp $TESTDIR/../files/02/fib.h .

  $ sed -i 's/int fib.*/#include "fib.h"/' main.c
  $ cat main.c
  #include <stdio.h>
  #include "fib.h"
  int main() { // Oops! main should be declared to return int.
    printf("Hello, %d jenga!\n", fib(10));
  }

  $ sed -i '1i#include "fib.h"' fib.c
  $ cat fib.c
  #include "fib.h"
  int fib(int x) {
    if (x < 2) return x;
    return fib(x-1) + fib(x-2);
  }

  $ jenga build -a 2>&1 | grep -v 'called at'
  A: gcc -Wall -c main.c -o main.o
  (directory) .
  (rule) main.o : main.c
  (command) $ gcc -Wall -c main.c -o main.o
  (stderr) main.c:2:10: fatal error: fib.h: No such file or directory
  (stderr)     2 | #include "fib.h"
  (stderr)       |          ^~~~~~~
  (stderr) compilation terminated.
  (exit-code) 1
  A: gcc -Wall -c fib.c -o fib.o
  (directory) .
  (rule) fib.o : fib.c
  (command) $ gcc -Wall -c fib.c -o fib.o
  (stderr) fib.c:1:10: fatal error: fib.h: No such file or directory
  (stderr)     1 | #include "fib.h"
  (stderr)       |          ^~~~~~~
  (stderr) compilation terminated.
  (exit-code) 1
  ran 2 commands
  Build failed for 2 reasons:
  (1) action failed for rule targeting: main.o
  (2) action failed for rule targeting: fib.o

Add missing dep to both compile rules
  $ sed -i 's/: main.c/: main.c fib.h/' build.jenga
  $ sed -i 's/: fib.c/: fib.c fib.h/' build.jenga
  $ cat build.jenga
  hello.exe : main.o fib.o
    gcc main.o fib.o -o hello.exe
  
  main.o : main.c fib.h
    gcc -Wall -c main.c -o main.o
  
  fib.o : fib.c fib.h
    gcc -Wall -c fib.c -o fib.o

  $ jenga build -a
  A: gcc -Wall -c main.c -o main.o
  A: gcc -Wall -c fib.c -o fib.o
  checked 3 targets
  ran 2 commands
