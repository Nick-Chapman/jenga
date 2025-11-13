
# More rules and more source files

This tutorial section works through an example with multiple build rules and multiple source files.

- [Here](files/02) are the files used in this section of the tutorial.
- [Here](cram/02_more_rules.t) is the companion cram file.

This example starts with two `.c` files and a `build.jenga` file containing three rules.
To follow along, work in a fresh directory, typing or copying in the example files as directed.

main.c
```
#include <stdio.h>
int fib(int);
void main() { // Oops! main should be declared to return int.
  printf("Hello, %d jenga!\n", fib(10));
}
```

fib.c
```
int fib(int x) {
  if (x < 2) return x;
  return fib(x-1) + fib(x-2);
}
```

build.jenga
```
hello.exe : main.o fib.o
  gcc main.o fib.o -o hello.exe

main.o : main.c
  gcc -c main.c -o main.o

fib.o : fib.c
  gcc -c fib.c -o fib.o
```

These build rules implement the standard approach of separate compilation and linking.
Each `.c` file is independently compiled to a `.o` file using `gcc -c`. Then the two `.o` files are linked together using plain `gcc`.

Note how each rule explicitly lists all of its dependencies. Any file which is required for the build action to run must be listed. If any dependencies are missing, then the compilation and hence the entire build will fail.
This is a good thing! Build systems which don't use sandboxing when running build actions may suffer from _accidental_ build success. This is a very bad thing. It means that if a file changes that was read by a build action but was not explicitly listed as a dependency, then a rebuild wont rerun that action, leading to incorrect builds. The developer nightmare!

Let's build this example.
Jenga responds by running the three actions in the correct order as determined by their dependencies.
```
$ jenga build
elaborated 3 rules and 3 targets
A: gcc -c fib.c -o fib.o
A: gcc -c main.c -o main.o
A: gcc main.o fib.o -o hello.exe
ran 3 actions
```

Let's run the executable. Unfortunately something is wrong. The exit code is 17.
```
$ ,jenga/hello.exe; echo $?
Hello, 55 jenga!
17
```

Turns out there is a bug in our `main.c`
We have even commented that fact in the code!
We'll fix that soon.
But first, lets switch up our compiler warnings so we get properly notified about our mistake.
We'll change both `gcc -c` actions to add the `-Wall` flag.
```
main.o : main.c
  gcc -Wall -c main.c -o main.o

fib.o : fib.c
  gcc -Wall -c fib.c -o fib.o
```

Rebuild. Jenga knows that even though we didn't modify any source file, the compile actions have changed and so must be rerun.
```
$ jenga build
elaborated 3 rules and 3 targets
A: gcc -Wall -c fib.c -o fib.o
A: gcc -Wall -c main.c -o main.o
main.c:3:6: warning: return type of ‘main’ is not ‘int’ [-Wmain]
    3 | void main() { // Oops! main should be declared to return int.
      |      ^~~~
ran 2 actions
```

Hurrah, the C compiler has pointed out our mistake.
Notice Jenga has not rerun the link step, so we can surmise that the generated `.o` files are unchanged.
If we wanted, we could add `-Werror` as well as `-Wall` which would cause the C compiler to produce a non-zero exit code.
This would be detected by jenga and treated as a build failure.

Lets fix the source code, rebuild and move on. Change `main.c` to read:
```
#include <stdio.h>
int fib(int);
int main() {
  printf("Hello, %d jenga!\n", fib(10));
}
```

Rebuild. Jenga will rerun the compile step for the changed `main.c` (source has changed) and the link step (`main.o` has changed):
```
$ jenga exec -a hello.exe
elaborated 3 rules and 3 targets
A: gcc -Wall -c main.c -o main.o
A: gcc main.o fib.o -o hello.exe
ran 2 actions
Hello, 55 jenga!
```

Let make one final change to this example.
It's common practise in C-code to `#include` a header file that provides a prototype for externally defined functions such as the call to `fib(10)` in `main.c`.

Currently `main.c` directly states the prototype `int fib(int)` which is bad style.
We shall place the prototype in a new file `fib.h` which can be included in both `main.c` and `fib.c`.
We include it in `fib.c` so the compiler can check there is no mismatch between the declaration and the definition of `fib`.

Here is what our source files look like now:

fib.h
```
int fib(int);
```

main.c
```
#include <stdio.h>
#include "fib.h"
int main() {
  printf("Hello, %d jenga!\n", fib(10));
}
```

fib.c
```
#include "fib.h"
int fib(int x) {
  if (x < 2) return x;
  return fib(x-1) + fib(x-2);
}
```

Unfortunately when we try to build, jenga will report an error.
```
$ jenga build
elaborated 3 rules and 3 targets
A: gcc -Wall -c fib.c -o fib.o
fib.c:1:10: fatal error: fib.h: No such file or directory
    1 | #include "fib.h"
      |          ^~~~~~~
compilation terminated.
```

The build has failed because the C-compiler has aborted with an error.
This happened because we failed to list the new file `fib.h` as a dependency of the rules which build `main.o` and `fib.o`.
Consequently, `fib.h` is simply not available to be read by the compiler.
Running in a sandbox has prevented accidental build success.

We must fix our build rules to explicitly list the dependencies on `fib.h`. We have two places to fix:
`build.jenga` now looks like this:
```
hello.exe : main.o fib.o
  gcc main.o fib.o -o hello.exe

main.o : main.c fib.h
  gcc -Wall -c main.c -o main.o

fib.o : fib.c fib.h
  gcc -Wall -c fib.c -o fib.o
```

Rebuild is successful.
```
$ jenga build
elaborated 3 rules and 3 targets
A: gcc -Wall -c fib.c -o fib.o
A: gcc -Wall -c main.c -o main.o
ran 2 actions
```

Continue the jenga tutorial by learning about
[build artifacts and the build cache](03_artifacts_and_cache.md).
