<!-- THIS FILE IS GENERATED AUTOMATICALLY. -->
<!-- DO NOT EDIT THIS FILE. -->
<!-- EDIT README.md.template INSTEAD. -->
# x86ISTMB v1.0.0


![CI Status](https://github.com/ethanuppal/cs3110_compiler/actions/workflows/ci.yaml/badge.svg)

> "x86 is simple trust me bro"  
> Last updated: 2024-05-11 03:02:13.582897

```
$ ./main -h
CS 3110 final project

Usage: ./main [-h|-v]
   or: ./main FILE [-g][-O]

-h,--help         prints this info
-v,--version      prints version info
-g,--gen          only produces IR
-O,--optimize     runs optimizations
-c,--compile      only produces object files
```
```
$ ./main -v
x86ISTMB v0.1.0

Written by: Utku Melemeti, Ethan Uppal, Jeffrey Huang, Jason Klein, Vijay Shanmugam
```

## Group

- Utku Melemetci (um44)
    - IR generation
    - Register allocation
    - Data structures
    - Randomized testing
- Ethan Uppal (eu55)
    - Static analysis
        - Types and type checking
        - Control flow
    - Live variable analysis
    - IR representation and abstraction for x86 assembly
    - Emission to executables following Sys V ABI, integrating C runtime
    - CLI/user interface and program driver
- Jeffrey Huang (jrh382)
    - Parser and lexer
    - Abstract syntax tree
    - Type checking

With minor contributions from
- Jason Klein (jak532)
- Vijay Shanmugam (vrs29)

## Usage

The `Makefile` in the project root directory enables easy usage.
Simply run `make` to build the project executable, `./main`.
`make clean` will clear all build and executable files.
Documentation is in [introduction.md](docs/introduction.md).
Installation instructions are in [INSTALL.md](INSTALL.md).
A full user manual is in [user_manual.md](docs/user_manual.md).
