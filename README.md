# x86ISTMB

![CI Status](https://github.com/ethanuppal/cs3110_compiler/actions/workflows/ci.yaml/badge.svg)

```shell
$ ./main -h
CS 3110 final project

Usage: ./main [-h|-v]
   or: ./main FILE [-g][-O]

-h,--help         prints this info
-v,--version      prints version info
-g,--gen          only produces IR
-O,--optimize     runs optimizations
$ ./main -v
x86ISTMB v0.1.0

Written by: Utku Melemeti, Jason Klein, Jeffrey Huang, Vijay Shanmugam, Ethan Uppal
```

## Group

This is the CS 3110 final project by:

- Jason Klein (jak532)
- Utku Melemetci (um44)
- Jeffrey Huang (jrh382)
- Vijay Shanmugam (vrs29)
- Ethan Uppal (eu55)

## Usage

The `Makefile` in the project root directory enables easy usage.
Simply run `make` to build the project executable, e.g., `./main --help`.
`make clean` will clear all build and executable files.
Documentation is in [introduction.md](docs/introduction.md).
Installation instructions are in [INSTALL.md](INSTALL.md)
