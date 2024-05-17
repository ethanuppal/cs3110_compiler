# User Manual

First, follow the instructions to [install](../INSTALL.md) our project.
Then, you can build and test the CLI tool using `make`.

```shell
make build
make test
```

Running `make build` will produce an executable in the same directory called `./main`.
You can see what options are available with `./main -h` or `./main --help`, and view versioning information with `./main -v` or `./main --version`.

Here are some other commands.

| Command    | Description            |
|------------|------------------------|
| `make clean` | Removes all build and executable files |
| `make docs` | Generates documentation |
| `make serve PORT=8003` | Hosts the documentation website at `localhost:8003` |
| `make bisect` | Runs `make test` and outputs the percentage of coverage (requires `pup` to be installed) |
| `make cloc` | Counts the lines of code in the project.

For documentation of the language, see [the wiki](https://github.com/ethanuppal/cs3110_compiler/wiki)
