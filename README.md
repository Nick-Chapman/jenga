# Jenga

Jenga is a powerful, language-agnostic build tool, providing safe, fast, incremental builds for anything!

Its defining features are:
1. Sandboxing.
2. Full history shared caching.
3. Powerful yet simple configuration.

Jenga was originally developed at Jane Street. [More information about the history](doc/history.md).
This version is a completely new rewrite.

Skip the features; jump directly to [getting started](tutorial/01_getting_started.md).


## Features

[How does Jenga compare with other build systems?](doc/comparison.md)

- __General Purpose__:
Jenga is entirely language-agnostic.
Build rules can be constructed for anything which can be run at the command line.
These rules can be written on a per-project basis or shared more widely.
Many programming languages come with integrated, language-specific build tooling.
This is nice right up until your project needs build automation for other stuff: other languages, test frameworks, documentation, deployments, etc.

- __Sandboxing__:
Jenga's builds are hermetic.
Every build action is run in a separate sandbox allowing access only to declared dependencies.
This ensures no dependency can be missed which would otherwise lead to an incorrect rebuild.
This is the core of Jenga's safety claim.

- __Caching__:
Jenga provides full history shared caching.
Every build at every source version is cached.
The same cache is shared across all projects and repo checkouts.
So long as the cache exists, no build action will ever be repeated.
This forms the basis of Jenga's fast, incremental rebuilds.
Jenga's caching is via _constructive traces_,
making use of _md5_ message digests and not file modification times (which is a fundamentally broken approach).

- __Powerful__:
The core interface to Jenga's build engine is a simple haskell [EDSL](src/Interface.hs) for describing build rules.
This provides dynamic rule generation and dynamic dependencies (a.k.a. monadic dependencies).

- __Simple__:
The rule construction interface is exposed to the Jenga operator via a familiar _make-style_ [DSL](doc/jenga-syntax.md)
whilst retaining the power of the underlying Haskell EDSL.
As a bonus, Jenga can exist as a standalone executable;
a Jenga operator does not require access to a Haskell development system.

- __Parallel__:
Jenga supports parallel builds via the `-j` command line option.
Or simply by running multiple instances of `jenga` at the command line.

- __Watcher mode__:
The wrapper script [`jenga`](./jenga) adds support for `-w` using `inotifywait`.
Other command line arguments are handled by the core `jenga.exe` executable.

## Prerequisites

To build Jenga from source requires:
- [Haskell](https://www.haskell.org)
- [Stack](https://docs.haskellstack.org/en/stable/)
- [ghcup](https://www.haskell.org/ghcup/) The easiest way to get Haskell and Stack is via Ghcup. Also the [haskell rules](rulelib/haskell.jenga) which allow jenga to build itelf require that `ghcup` was used to install `ghc`.
- [unbuffer](https://command-not-found.com/unbuffer) (if installed) is used by the haskell rules to force `ghc` to show coloured output.
- [inotifywait](https://linux.die.net/man/1/inotifywait) is needed for the jenga wrapper script to support watcher mode: `sudo apt install inotify-tools`.


## Installation

Choose a permanent directory into which the jenga sources will be cloned; perhaps: `~/jenga`.

### Clone the jenga sources (first time only)
```
git clone git@github.com:Nick-Chapman/jenga.git ~/jenga
```

### Pull the latest jenga sources (subsequent times)
```
(cd ~/jenga; git pull)
```

Choose where to install the jenga executable and wrapper script; perhaps `~/.local/bin`.

### Install with watcher mode support (`-w`); requires `inotifywait`
```
(cd ~/jenga/src; stack run -- install jenga.exe ~/.local/bin/jenga.exe)
ln -sf ~/jenga/jenga ~/.local/bin/jenga
```

### Install without watcher mode support
```
(cd ~/jenga/src; stack run -- install jenga.exe ~/.local/bin/jenga.exe)
ln -sf jenga.exe ~/.local/bin/jenga
```

Now we can display Jenga's help message.
```
jenga --help
```

## Tutorial

[tutorial index](tutorial/README.md) to learn how to setup and run jenga builds.
