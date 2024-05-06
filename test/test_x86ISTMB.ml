(** Test Plan:

    X86ISTMB is thoroughly tested using a variety of black-box and randomized
    testing approaches. We have unit tests for many of our main data structures,
    randomized property-based testing using QCheck and an AST fuzzer for others,
    and a suite of snapshot tests for testing end-to-end behavior. If at any
    point during development we did manual testing and found incorrect behavior,
    we added unit/snapshot regression tests as fit, so no parts of the system
    that were manually tested and found to be incorrect at one point were not
    also covered by our test suite.

    Unit tests: we have unit tests for [Context], [Digraph], [Id], and [Util],
    which we used to verify the behavior of compilation units for data
    structures and helper functions that could be tested in isolation like this.

    Randomized property-based testing: to better ensure correct behavior of
    state-dependent code like [Context], we used QCheck to generate random
    contexts (effectively random stacks of symbol-to-value mappings) and then
    test properties like the invariance of a [push] followed by a [pop] and
    [insert] followed by a [get].

    Snapshot testing: to test end-to-end behavior, we have a suite of snapshot
    tests. These take files as input, perform a transform, and then compare the
    output to another "expected output" file. We use snapshot tests to test that
    parsing works for many different cases, to test that our IR optimizations
    like branch elimination and constant folding work as expected, and that type
    inference to ensure we catch edge cases in our [Analyzer]

    AST fuzzer: randomizing an AST with QCheck could create ASTs that don't
    actually correspond to valid programs, which would make testing the analyzer
    impossible and testing the IR optimizations a lot slower (since we'd have to
    discard every invalid AST). Fuzzing ASTs with mutations always generates
    ASTs that correspond to valid programs. We use the fuzzer mainly as a way to
    test invariants of the IR, which aren't as heavily enforced by the OCaml
    type system as the analyzer.

    This testing approach demonstrates the correctness of the system in a
    variety of ways. We use unit testing (with randomization) to show
    correctness of individual components, and snapshot testing to verify that
    the end-to-end behavior is what we expect.

    Note 1: We're using Alcotest instead of OUnit by express permission of Prof.
    Clarkson (screenshot of private Ed post available at
    https://tinyurl.com/alcotest-permission).

    Note 2: While we might not have 50 hard-coded tests, we have hundreds of
    checks that end up running as a result of our QCheck-powered tests and
    snapshot testing. *)

let () =
  [
    Test_util.test_suite;
    Test_id.test_suite;
    Test_snapshots.ir_suite;
    Test_snapshots.type_suite;
    Test_snapshots.basic_suite;
    Test_digraph.test_suite;
    Test_liveliness.test_suite;
    Test_context.suite;
  ]
  |> Alcotest.run "x86ISTMB"
