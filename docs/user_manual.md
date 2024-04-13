# User Manual

First, follow the instructions to [install](../INSTALL.md) our project.
Then, you can build and test the CLI tool using `make`.

```shell
make build
make test
```

Running `make build` will produce an executable in the same directory called `./main`.
You can see what options are available with `./main -h` or `./main --help`, and view versioning information with `./main -v` or `./main --version`.

To clean up all build and executable files, run `make clean`.

To generate documentation, run `make docs`.
Then, view it locally with `make serve PORT=8003`, which will host the documentation website at `localhost:8003`.
