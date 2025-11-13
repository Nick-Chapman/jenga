
# Getting started with jenga

Following a successful download and install of jenga, this tutorial will guide you through your first steps in writing jenga rules and running jenga builds.

- [Here](files/01) are the files used in this section of the tutorial.
- [Here](cram/01_getting_started.t) is the companion cram file.

## Download, build and install jenga

The following steps will download and build `jenga` from source.
```
rm -rf /tmp/jenga
git clone https://github.com/Nick-Chapman/jenga.git /tmp/jenga
cd /tmp/jenga
stack build
```

This tutorial assumes you have `jenga` available in your path.
For example, you might choose to link a file `jenga`, in a directory picked up by your path, to the executable built by stack command above.
```
cd ~/.local/bin
ln -s $(find /tmp/jenga/.stack-work/dist -type f -name jenga) jenga
```

Now we can display jenga's top level usage/help message.
```
jenga --help
```

## First build

Let's use jenga to build and run a simple C application.

Here's our single source file `main.c`.
```
#include <stdio.h>
int main() {
  printf("Hello, jenga world!\n");
}
```
And here's our single build rule in the file `build.jenga`.
```
hello.exe : main.c
  gcc -o hello.exe main.c
```

This rule says that the _target_ `hello.exe` depends (`:`) on the source file `main.c`
To create the output target from its input, Jenga must run the compilation _action_
`gcc -o hello.exe main.c`.

Note the action must be indented from the preceeding line.
This follows a longstanding syntax used by `make`.
The action will be passed to by `bash` for execution when Jenga deems it necessary.

In general, the format of a simple rule in a `.jenga` file is:
```
LIST-OF-TARGETS : LIST-OF-DEPENDENCIES
  ACTION
```
Let's work in a fresh directory to try this out.
First type or copy in the example files.
```
mkdir /tmp/j1
cd /tmp/j1
cp /tmp/jenga/tutorial/files/01/main.c .
cp /tmp/jenga/tutorial/files/01/build.jenga .
```

Then run `jenga build` in the new directory.
```
$ jenga build
elaborated 1 rule and 1 target
A: gcc -o hello.exe main.c
ran 1 action
```

The executable been placed in the `,jenga/` artifacts directory, which we can run directly, or
we can use `jenga exec -a <TARGET>` to combine the build and execution of a target.

This has the same behaviour as `jenga build && ,jenga/<TARGET>`
(The `-a` flags shows the build info messages, which would otherwise be hidden by plain `jenga exec`).
```
$ jenga exec -a hello.exe
elaborated 1 rule and 1 target
Hello, jenga world!
```

Let's make a change to our source code:
edit `main.c` replacing the string `world` with `universe`.
Rebuild and rerun.

Jenga will rerun the `gcc` action because the input file `main.c` has changed.
```
$ jenga exec -a hello.exe
elaborated 1 rule and 1 target
A: gcc -o hello.exe main.c
ran 1 action
Hello, jenga universe!
```

Revert the example back to its original state, replacing `universe` with `world`.
Rebuild and rerun again.

This time, no build actions are run.
Yet the executable correctly reverts to printing the original message.
This desirable behaviour is an effect of _full caching_.
```
$ jenga exec -a hello.exe
elaborated 1 rule and 1 target
Hello, jenga world!
```

Continue the jenga tutorial by learning how to setup builds with
[more rules and more source files](02_more_rules.md).
