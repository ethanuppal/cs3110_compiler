# Installation Instructions

> [!NOTE]
> This document is not a user manual. 
> It simply explains how to get the project onto your computer.
> To see how to get it up and running, read the [user manual](docs/user_manual.md).

These instructions assume you already have OCaml installed.
1. Create a new `opam` switch by running `opam switch create cs3110-compiler ocaml-base-compiler.5.1.1`
2. Install the required libraries: `opam install menhir batteries ounit2`
3. Build the project by running `dune build`
4. Use `dune exec bin/main.exe -- -h` to see usage instructions. Replace `-h` in the previous command with the flags you want to use.
