
# Dynamic dependencies

This section explores the use of dynamic dependencies in jenga rules.

- [Here](files/05) are the files used in this section of the tutorial.
- [Here](cram/05_dynamic_deps.t) is the companion cram file.

In our running example, here was the previous state of `build.jenga`
```
hello.exe : main.o fib.o
  gcc main.o fib.o -o hello.exe

main.o : main.c fib.h
  gcc -Wall -c main.c -o main.o

fib.o : fib.c fib.h
  gcc -Wall -c fib.c -o fib.o
```

This works, and is fine for a tiny fixed example like this. But in bigger examples it becomes very tedious to explicitly list all dependencies for the compilation rules.
Specifically: `main.o : main.c fib.h` and `fib.o : fib.c fib.h`

Currently we figured out these dependencies by manually inspecting the source files to see
which header files are `#include`d by which `.c` files.
But `gcc` (as do many compilers) provides us with a way to automate this process.

By running `gcc` with specific flags (`-MG -MM`) it will produce a file which lists the discovered dependencies. Jenga can make use of this file to allow us automate and simplify the build rule descriptions.

This is our new `build.jenga`.
```
hello.exe : main.o fib.o
  gcc main.o fib.o -o hello.exe

main.o : @depends
  gcc -Wall -c main.c -o main.o

fib.o : @depends
  gcc -Wall -c fib.c -o fib.o

depends : main.c fib.c
  gcc -MG -MM *.c > depends
```

There is a new rule which describes a target named `depends` which is built by running `gcc -MG -MM`.
Also, `@depends` is listed as the single dependency for both `main.o` and `fib.o`.
How this works will follow.

First lets build. We use `-f` so we can view the actions.
```
$ jenga build -f
using temporary cache: /tmp/.cache/jenga/619096
elaborated 4 rules and 4 targets
A: gcc -MG -MM *.c > depends
A: gcc -Wall -c fib.c -o fib.o
A: gcc -Wall -c main.c -o main.o
A: gcc main.o fib.o -o hello.exe
ran 4 actions

$ jenga run example/hello.exe
Hello, 55 jenga!
```

The build actions looks similar to before, but with the addition of an action which created a target file called `depends`. Lets inspect that.
```
$ cat ,jenga/example/depends
fib.o: fib.c fib.h
main.o: main.c fib.h
```

This is what `gcc -MG -MM` has kindly output for us.
It tells us explicitly the dependencies it discovered for `fib.o` and `main.o`

We make use of this file in the build rules for `fib.o` and `main.o`.
For example.
```
main.o : @depends
  gcc -Wall -c main.c -o main.o
```

The `@` syntax marks dynamic dependencies, and tells Jenga that the dependencies for `main.o` can be found by _first_ building the file `depends` and then _reading_ that file to discover what the dependencies actually are.

Only dependencies on lines relevant for `main.o` are used. Specifically `main.c` and `fib.h` (but not `fib.c`).
This is exactly what we want!

If we run `jenga build --list-rules` or `jenga build -r`,
Jenga will show us exactly what the build rules look like once all dynamic dependencies are resolved.
```
$ jenga build --list-rules
elaborated 4 rules and 4 targets
example/depends : example/main.c example/fib.c
  cd example ; gcc -MG -MM *.c > depends

example/fib.o : example/fib.c example/fib.h
  cd example ; gcc -Wall -c fib.c -o fib.o

example/main.o : example/main.c example/fib.h
  cd example ; gcc -Wall -c main.c -o main.o

example/hello.exe : example/main.o example/fib.o
  cd example ; gcc main.o fib.o -o hello.exe
```

If we list the rules from within the `example` directory, they are somewhat more concise, but provide exactly the same information.
```
$ (cd example; jenga build --list-rules)
elaborated 4 rules and 4 targets
depends : main.c fib.c
  gcc -MG -MM *.c > depends

fib.o : fib.c fib.h
  gcc -Wall -c fib.c -o fib.o

main.o : main.c fib.h
  gcc -Wall -c main.c -o main.o

hello.exe : main.o fib.o
  gcc main.o fib.o -o hello.exe
```

To produce the rule listing, Jenga must do a partial build.
We can see this if we add the `-f` flag.
Now it is explicit that jenga had to build the `depends` target.
```
using temporary cache: /tmp/.cache/jenga/225203
elaborated 4 rules and 4 targets
A: gcc -MG -MM *.c > depends
depends : main.c fib.c
  gcc -MG -MM *.c > depends

fib.o : fib.c fib.h
  gcc -Wall -c fib.c -o fib.o

main.o : main.c fib.h
  gcc -Wall -c main.c -o main.o

hello.exe : main.o fib.o
  gcc main.o fib.o -o hello.exe
ran 1 action
```

## Summary

By making use of dynamic dependencies (`@`), the build rules will automatically adapt to changes in header file inclusion.

What we should also like is for the build rules to automatically adapt to the addition, renaming and removal of `.c` files. This requires dynamic rule generation and is covered in the following tutorial section.


Continue the jenga tutorial by learning about
[dynamic rule generation](06_dynamic_rules.md).
