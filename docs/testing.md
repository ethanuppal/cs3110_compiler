# Testing

As described in the [user manual](user_manual.md), all testing can be run with `make test`.
By permission (and interest) of Professor Clarkson, we are using the framework Alcotest instead of OUnit2 (see https://tinyurl.com/alcotest-permission).

## Plan

TODO

## Snapshot Testing

> [!NOTE]
> This section is slightly outdated.

To ensure that internal workings are correct between edits, we have a system of snapshot testing.
Based on the version in [`Meta.get.version`](../lib/meta.ml), a different suite of input-output tests will be read from the [`test/snapshots`](../test/snapshots/).
The snapshot tester will run the interpreter on all the `*.in` files and compare the standard output resulting from the execution to the corresponding `.out` file.
If they do not match, a testing error is indicated.
