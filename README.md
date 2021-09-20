# micro-c-analyzer

This repository contains a program analysis module for the **MicroC** language.
## Installation

We depend on BNFC, Alex and Happy:

```shell
stack install BNFC
stack install alex
stack install happy
```

After you install the dependencies, run:

```shell
stack build # build
stack run   # build and execute the program
stack test  # build and run the test suite
```

## Documentation

To generate [Haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html) documentation for the project, run:

```shell
stack haddock --keep-going --open micro-c-analyzer
```