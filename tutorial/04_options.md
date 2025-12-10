
# Controlling jenga at the command line

This section explore some of Jenga's command line options which control how the build is performed.

- [Here](files/04) are the files used in this section of the tutorial.

The examples files are unchanged from the previous section of the tutorial.

## Help (`jenga -h`)

See the full command line options with
```
$ jenga --help
$ jenga build --help
```

## Cache control (`jenga build -c DIR`)

We saw previously how the cache can be selected with `--cache` (or `-c`).

For example to use a `.cache` in the current directory.
```
$ jenga build -c.
elaborated 3 rules and 3 targets
A: gcc -Wall -c -o fib.o fib.c
A: gcc -Wall -c -o main.o main.c
A: gcc -o hello.exe main.o fib.o
ran 3 actions
```

## Temporary Cache (`jenga build -f`)

If we would like to see what actions would be run if nothing is cached, we can make use of the
convenient `--temporary-cache` (or `-f`) flag.
This is achieved by having Jenga use a brand new temporary cache named for the processes _pid_.
Jenga reports to us the directory it used.

```
$ jenga build -f
using temporary cache: /tmp/.cache/jenga/500351
elaborated 3 rules and 3 targets
A: gcc -Wall -c -o fib.o fib.c
A: gcc -Wall -c -o main.o main.c
A: gcc -o hello.exe main.o fib.o
ran 3 actions
```

## Target and rule discovery (`jenga build -t|-r`)

We can ask jenga to tell us exactly what targets it knows how to build, and the rules it would use to build them.

Discover targets with `jenga build --list-targets` (or `jenga build -t`).
```
$ jenga build --list-targets
example/fib.o
example/main.o
example/hello.exe
```

Discover rules with `jenga build --list-rules` (or `jenga build -r`).
```
$ jenga build --list-rules
example/fib.o : example/fib.c example/fib.h
  cd example ; gcc -Wall -c -o fib.o fib.c

example/main.o : example/main.c example/fib.h
  cd example ; gcc -Wall -c -o main.o main.c

example/hello.exe : example/main.o example/fib.o
  cd example ; gcc -o hello.exe main.o fib.o
```

For the current example this information is quite obvious just by looking at the `build.jenga` files.
But when the build rules make use of dynamic dependencies and dynamic rule generation, these options are very useful.


## Limiting the scope of the build (`jenga build [DIRS]`)

By default Jenga will build using all `build.jenga` files discovered recursively from the current directory.

For example, suppose we duplicate our example directory.
```
$ cp -rp example copied

$ jenga build
elaborated 6 rules and 6 targets

$ jenga build -t
example/fib.o
example/main.o
example/hello.exe
copied/fib.o
copied/main.o
copied/hello.exe
```

We can limit what is built by listing starting directories on the command line.
```
$ jenga build example
elaborated 3 rules and 3 targets

$ jenga build example -t
example/fib.o
example/main.o
example/hello.exe
```

## Parallel builds (`jenga build -j [NUM_PROCS]`)

Remember that use of the `-f` flag force all build actions to be run.

Because the `example` and `copied` directories are identical,
even with a fresh cache, Jenga only had to run 3 actions to create all 6 targets.

```
$ jenga build -f
using temporary cache: /tmp/.cache/jenga/501752
elaborated 6 rules and 6 targets
A: gcc -Wall -c -o fib.o fib.c
A: gcc -Wall -c -o main.o main.c
A: gcc -o hello.exe main.o fib.o
ran 3 actions
```

The `-j` flag requests parallel builds.
Here we combine `-j2` with `-f` to force the build actions to be run.
Even on this tiny examples we gain some parallelism.
```
$ jenga build -fj2
using temporary cache: /tmp/.cache/jenga/502396
elaborated 6 rules and 6 targets
elaborated 6 rules and 6 targets
A: gcc -Wall -c -o fib.o fib.c
A: gcc -Wall -c -o main.o main.c
A: gcc -o hello.exe main.o fib.o
ran 1 action
ran 2 actions
```

To see which process is doing what work, we can combine with the `--show-pid` (or `-p`) flag.
```
$ jenga build -fj2 --show-pid
using temporary cache: /tmp/.cache/jenga/502991
[502993] elaborated 6 rules and 6 targets
[502991] elaborated 6 rules and 6 targets
[502993] A: gcc -Wall -c -o fib.o fib.c
[502991] A: gcc -Wall -c -o main.o main.c
[502991] A: gcc -o hello.exe main.o fib.o
[502993] ran 1 action
[502991] ran 2 actions
```

This makes it clear that the two compile actions are run in parallel, followed afterwards by the dependant link action.


Continue the jenga tutorial by learning about
[dynamic dependencies](05_dynamic_deps.md).
