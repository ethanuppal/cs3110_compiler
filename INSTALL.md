# Installation Instructions

> [!NOTE]
> This document is not a user manual.
> It simply explains how to get the project onto your computer and running.
> To see how to use the compiler, check out the [user manual](docs/user_manual.md).

## Install System Dependencies
These instructions assume you already have OCaml and `make` installed.

You'll need `nasm` for compiling assembly and `clang` for compiling and linking the runtime. The recommended way to install these on Linux and Max is through [Homebrew](https://brew.sh/).

Once you have Homebrew, run `brew install llvm nasm`. If you don't want to use Homebrew,
you're welcome to install the dependencies on your own, but they must be in `PATH`.

## Build and Run
1. Create a new `opam` switch by running `opam switch create cs3110-compiler ocaml-base-compiler.5.1.1`
2. Install the required libraries: `make deps`
3. Build the project by running `make`. A main executable will be copied into your directory.
4. Use `./main -h` to see usage instructions. Replace `-h` in the previous command with the flags you want to use.