# Jenga

Jenga is a powerful, language agnostic build tool, providing safe, fast, incremental builds for anything!

It's defining features are:
1. Sandboxing.
2. Full history shared caching.
3. Powerful yet simple configuration.

Jenga was originally developed at Jane Street. [More information about the history](doc/history.md).
This version is a completely new rewrite.

Skip the why, and jump directly to [getting started](tutorial/01_getting_started.md).


## Features

[How does Jenga compares with other build systems?](doc/comparison.md)

- __General Purpose__:
Jenga is a entirely language agnostic.
Build rules can be constructed in Jenga's make-style DSL for anything which can be run at the command line.
These rules can be written on a per-project basis or shared more widely,
Many programming languages (scala, rust, golang, haskell) come with integrated, language-specific build tooling.
This is nice right up until your project needs build automation for other stuff: other languages, test frameworks, documentation, deployments, etc.

- __Sandboxing__:
Jenga's builds are hermetic.
Every build action is run in a separate sandbox allowing access only to declared dependencies.
This ensure no dependencies can be missed, which would otherwise lead to incorrect rebuilds.
This is the core of Jenga's safety claim.

- __Caching__:
Jenga's provides full history shared caching.
Every build at every source version is cached between all builds/projects which use the same cache.
So long as the cache exists, no build action will ever be repeated.
This forms the basis of Jenga's fast, incremental rebuilds.
Jenga's caching is via _constructive traces_,
Making use of _md5_ message digests and not file modification times (which is a fundamentally broken approach).

- __Powerful__:
The core interface to Jenga's build engine is a simple haskell [EDSL](src/Interface.hs) to describe build rules,
This provides dynamic rule generation, and dynamic dependencies (also known as monadic dependencies).

- __Simple__:
The core rule construction interface is exposed to the Jenga operator via a familiar _make-style_ [DSL](doc/jenga-syntax.md)
all the while retaining the power of the underlying Haskell EDSL.
As a bonus, Jenga can exist as standalone executable;
a Jenga operator does not require access to a Haskell development system.

- __Parallel__:
Jenga supports parallel builds via the `-j` command line option.
Or simply by running multiple instances of `jenga` at the command line.

- __Watcher mode__:
Jenga has no integrated support,
but a rudimentary watcher mode is possible via `inotifywait`,
as provided by the [`jenga-w`](jenga-w) wrapper script.


## Prerequisites

To build Jenga from source requires:
- [Haskell](https://www.haskell.org).
- [Stack](https://docs.haskellstack.org/en/stable/)
- [Cram](https://bitheap.org/cram/). Optional for testing.
- [ghcup](https://www.haskell.org/ghcup/). The easiest way to get Haskell and Stack is via Ghcup.


## Tutorial

Once you have `ghc` and `stack` installed, please proceed to the
[tutorial index](tutorial/index.md) to get `jenga` built and installed,
and to learn how to setup and run jenga builds.
