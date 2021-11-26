# micro-c-analyzer

This repository contains a program analysis module for the **MicroC** language.

## Building

Make sure you have installed [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

Execute the following commands to install the dependencies and build the project:

```shell
# Install dependencies
stack install BNFC  # BNF Converter
stack install alex  # lexer generator
stack install happy # parser generator
# Generate the parser
make
# Build the project - first build might take up to a few minutes
stack build
```

### NOTE on the Makefiles

Two files, `Makefile` and `GNUmakefile` are provided.

The former is for the Windows operating system and has a default target that will generate the parser files.

The latter is supposed to be used on Unix systems and additionally contains various targets for profiling the application. The required external dependencies can be installed with `stack install`.

## Running

The analysis module provides a simple CLI with two commands:
* `benchmark` -- executes the benchmarks described in the report
* `analyse` -- runs an analysis on a MicroC source file

The syntax for executing the program is as follows:

```shell
stack run -- benchmark
# or
stack run -- analyse [analysis] [algorithm] <path to source file>
```

Running the program with the `-h/--help` flag will show an appropriate help message:

```shell
$ stack run -- analyse -h

Usage: micro-c-analyzer analyse ANALYSIS ALGORITHM PATH
  Run an analysis on a MicroC source file

Available options:
  -h,--help                Show this help text
  ANALYSIS                 One of: dangerous, faint, interval, live, reaching,
                           signs
  ALGORITHM                One of: chaotic, naive, pending-set, post-order,
                           queue, stack
  PATH                     Path to the MicroC source file
```

## Tests

To execute the test suite, run `stack test`.
To also generate a test coverage report, add the `--coverage` flag.

## Documentation

To generate [Haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html) documentation for the project, run `make docs`.
