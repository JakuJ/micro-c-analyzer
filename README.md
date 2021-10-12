# micro-c-analyzer

This repository contains a program analysis module for the **MicroC** language.

## Installation

We depend on BNFC, Alex and Happy:

```shell
stack install BNFC
stack install alex
stack install happy
```

After you install the dependencies, run any of:

```shell
stack build             # build the project
stack test [--coverage]   # run the test suite [and generate an HTML report]
```

## Documentation

To generate [Haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html) documentation for the project, run:

```shell
stack haddock --keep-going --open micro-c-analyzer
```
