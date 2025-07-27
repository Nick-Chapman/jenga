
This cram file accompanies the jenga tutorial.

Get a jenga executable
  $ (cd $TESTDIR/../..; jenga build src -q) && ln $TESTDIR/../../,jenga/src/jenga jenga.exe

Make a small script to run jenga with a local cache.
Avoiding interference from the global cache, which will make this test non-deterministic.

  $ echo 'exec ./jenga.exe "$@" --cache=.' > jenga
  $ chmod +x jenga
  $ export PATH=.:$PATH

Get the example.

  $ cp -rp $TESTDIR/../files/02/build.jenga .
  $ cp -rp $TESTDIR/../files/02/main.c .
  $ cp -rp $TESTDIR/../files/02/fib.c .

Initial build. Expect 3 actions to be run

  $ jenga build
  elaborated 3 rules and 3 targets
  A: gcc -c fib.c -o fib.o
  A: gcc -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  ran 3 actions

Running executable returns exit code 17

  $ ,jenga/hello.exe; echo $?
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

  $ jenga build
  elaborated 3 rules and 3 targets
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  main.c:3:6: warning: return type of 'main' is not 'int' [-Wmain]
      3 | void main() { // Oops! main should be declared to return int.
        |      ^~~~
  ran 2 actions

Fix code. Compile of main.c and link are rerun

  $ sed -i 's/void main/int main/' main.c
  $ cat main.c
  #include <stdio.h>
  int fib(int);
  int main() { // Oops! main should be declared to return int.
    printf("Hello, %d jenga!\n", fib(10));
  }
  $ jenga build && ,jenga/hello.exe
  elaborated 3 rules and 3 targets
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  ran 2 actions
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

  $ jenga build 2>&1 | grep -v 'called at'
  elaborated 3 rules and 3 targets
  A: gcc -Wall -c fib.c -o fib.o
  fib.c:1:10: fatal error: fib.h: No such file or directory
      1 | #include "fib.h"
        |          ^~~~~~~
  compilation terminated.
  ExitFailure 1
  A: gcc -Wall -c main.c -o main.o
  main.c:2:10: fatal error: fib.h: No such file or directory
      2 | #include "fib.h"
        |          ^~~~~~~
  compilation terminated.
  ExitFailure 1
  ran 2 actions
  Build failed for 2 reasons:
  (1) 'fib.o': action failed for rule 'build.jenga:7'
  (2) 'main.o': action failed for rule 'build.jenga:4'

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

  $ jenga build
  elaborated 3 rules and 3 targets
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  ran 2 actions
