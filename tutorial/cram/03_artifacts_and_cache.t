
This cram file accompanies the jenga tutorial.

Get the example.

  $ cp -rp $TESTDIR/../files/03 example

See files

  $ cat example/fib.h
  int fib(int);

  $ cat example/main.c
  #include <stdio.h>
  #include "fib.h"
  int main() {
    printf("Hello, %d jenga!\n", fib(10));
  }

  $ cat example/fib.c
  #include "fib.h"
  int fib(int x) {
    if (x < 2) return x;
    return fib(x-1) + fib(x-2);
  }

  $ cat example/build.jenga
  hello.exe : main.o fib.o
    gcc main.o fib.o -o hello.exe
  
  main.o : main.c fib.h
    gcc -Wall -c main.c -o main.o
  
  fib.o : fib.c fib.h
    gcc -Wall -c fib.c -o fib.o

  $ find .
  .
  ./example
  ./example/fib.c
  ./example/main.c
  ./example/fib.h
  ./example/build.jenga

Get an up-to-date jenga executable in path, which runs with a local cachee

  $ (cd $TESTDIR/../..; jenga build src -q)
  $ echo exec $TESTDIR/../../src/jenga.exe '"$@"' > jenga
  $ chmod +x jenga
  $ export PATH=$PWD:$PATH

Initial build. Expect 3 actions to be run

  $ jenga build -a -c.
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 3 targets
  ran 3 commands

Zero build

  $ jenga build -a -c.
  checked 3 targets

  $ jenga build -mq && find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/hello.exe
  ,jenga/example/main.o

  $ cd example
  $ jenga build -a
  checked 3 targets
  $ jenga build -mq && find ,jenga
  ,jenga
  ,jenga/fib.o
  ,jenga/hello.exe
  ,jenga/main.o
  $ cd ..

Specifying a build cache

  $ jenga build -a --cache=tmp
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 3 targets
  ran 3 commands

  $ jenga build -m -a --cache=tmp
  checked 3 targets

  $ find tmp/.cache/jenga/files
  tmp/.cache/jenga/files
  tmp/.cache/jenga/files/83f35fc3965c22be4e45c16356b74c5b
  tmp/.cache/jenga/files/0fcce4811e995a71fe45c2826bb0868b
  tmp/.cache/jenga/files/d80b73c78daf9d8c4508a5959bcaef2a
  tmp/.cache/jenga/files/47a0ee09b975f7501dbeb5431b76c24c
  tmp/.cache/jenga/files/3f76f8b56f5f210a58391a85a90df90c
  tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242
  tmp/.cache/jenga/files/2b669a2f7d781171abadfc53ff38d0fd

  $ cat tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242
  int fib(int);

  $ md5sum tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242
  2360cef3c9dd4578f441193f7fd17242  tmp/.cache/jenga/files/2360cef3c9dd4578f441193f7fd17242

  $ md5sum ,jenga/example/hello.exe
  0fcce4811e995a71fe45c2826bb0868b  ,jenga/example/hello.exe

  $ tmp/.cache/jenga/files/0fcce4811e995a71fe45c2826bb0868b
  Hello, 55 jenga!

  $ find tmp/.cache/jenga/traces
  tmp/.cache/jenga/traces
  tmp/.cache/jenga/traces/a8eaac84a3846346e595f63a268ffc45
  tmp/.cache/jenga/traces/51c3df6cc40e72832038021c981b78f4
  tmp/.cache/jenga/traces/db6b016a94155599e02bf3102c2c5467

  $ cat tmp/.cache/jenga/traces/a8eaac84a3846346e595f63a268ffc45
  TRACE [RUN {exitCode = ExitSuccess, stdout = "", stderr = ""}] (Just [("hello.exe","0fcce4811e995a71fe45c2826bb0868b")])


Double build

  $ cp -rp example copied
  $ jenga build -a -c.
  checked 6 targets

What are the targets?

  $ jenga build -a --list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe
  copied/fib.o
  copied/main.o
  copied/hello.exe

  $ jenga build -a --list-targets -c.
  example/fib.o
  example/main.o
  example/hello.exe
  copied/fib.o
  copied/main.o
  copied/hello.exe

Controlling the scope of what to build

  $ jenga build -a copied -c.
  checked 3 targets

  $ jenga build -a --list-targets copied -c.
  copied/fib.o
  copied/main.o
  copied/hello.exe

Builds are relative
  $ cd copied
  $ jenga build -a -c..
  checked 3 targets
  $ cd ..

Using a non-default cache:

  $ jenga build -a --cache=my-cache
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 6 targets
  ran 3 commands

Using a non-default cache (still get minimal builds)

  $ jenga build -a --cache=my-cache
  checked 6 targets

Using a temporary cache with -f. Forces run of all the actions

  $ jenga build -a -f | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
  using temporary cache: /tmp/.cache/jenga/$$
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 6 targets
  ran 3 commands

  $ jenga build -a -f | sed 's|/tmp/.cache/jenga/[0-9]*|/tmp/.cache/jenga/$$|'
  using temporary cache: /tmp/.cache/jenga/$$
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 6 targets
  ran 3 commands

Where are the targets? ,jenga dir is created relative to where the build started

  $ jenga build -m -a
  checked 6 targets
  $ find ,jenga
  ,jenga
  ,jenga/example
  ,jenga/example/fib.o
  ,jenga/example/hello.exe
  ,jenga/example/main.o
  ,jenga/copied
  ,jenga/copied/fib.o
  ,jenga/copied/hello.exe
  ,jenga/copied/main.o

  $ jenga build -m -a copied
  checked 3 targets
  $ find ,jenga
  ,jenga
  ,jenga/copied
  ,jenga/copied/fib.o
  ,jenga/copied/hello.exe
  ,jenga/copied/main.o

  $ (cd copied && jenga build -m)
  checked 3 targets
  $ find copied/,jenga
  copied/,jenga
  copied/,jenga/fib.o
  copied/,jenga/hello.exe
  copied/,jenga/main.o

Artifacts are hardlinked to files in the cache (and each other)

  $ jenga build -m -a -c.
  checked 6 targets

Hardlink counts of 3 -- example,copied,.cache

  $ find ,jenga -type f | xargs stat -c "%h %n"
  3 ,jenga/example/fib.o
  3 ,jenga/example/hello.exe
  3 ,jenga/example/main.o
  3 ,jenga/copied/fib.o
  3 ,jenga/copied/hello.exe
  3 ,jenga/copied/main.o

Hardlink counts of 2 -- example,.cache

  $ rm -rf copied
  $ jenga build -m -a -c.
  checked 3 targets

  $ find ,jenga -type f | xargs stat -c "%h %n"
  2 ,jenga/example/fib.o
  2 ,jenga/example/hello.exe
  2 ,jenga/example/main.o

Hardlink counts of 1 -- example

  $ rm -rf .cache
  $ find ,jenga -type f | xargs stat -c "%h %n"
  1 ,jenga/example/fib.o
  1 ,jenga/example/hello.exe
  1 ,jenga/example/main.o

Rebuild, hardlink counts back to 2

  $ jenga build -m -a -c.
  A: gcc -Wall -c fib.c -o fib.o
  A: gcc -Wall -c main.c -o main.o
  A: gcc main.o fib.o -o hello.exe
  checked 3 targets
  ran 3 commands

  $ find ,jenga -type f | xargs stat -c "%h %n"
  2 ,jenga/example/fib.o
  2 ,jenga/example/hello.exe
  2 ,jenga/example/main.o
