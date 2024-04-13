let interpreter_transform path input =
  ignore path;
  let open Cs3110_compiler in
  let interpreter = Interpreter.create () in
  let stdout = ref "" in
  let statements = Parse_lex.lex_and_parse input in
  interpreter.set_mode (Text stdout);
  List.iter interpreter.step statements;
  stdout

let () = ignore interpreter_transform

let ir_transform path input =
  ignore path;
  ignore input;
  ""

let id_module_test_suite =
  let open Alcotest in
  let id_gens_isolated_test n () =
    let open Cs3110_compiler in
    let gen1 = Id.Gen.make () in
    let gen2 = Id.Gen.make () in
    (check @@ neg int)
      "Generators should have *different* self-ids. For some reason, Alcotest \
       doesn't display this differently than normal (issue #300)."
      (Id.Gen.id_of gen1) (Id.Gen.id_of gen2);
    ignore (Id.Gen.next gen1);
    (check int) "Using one generator should not affect the other."
      (Id.Gen.next gen1 - 1)
      (Id.Gen.next gen2);
    ignore (Id.Gen.next gen2);
    for _ = 1 to n do
      (check int) "Using generators in parallel should yield equality of ids."
        (Id.Gen.next gen1) (Id.Gen.next gen2)
    done
  in
  let create_id_gen_isolation_test n =
    test_case
      (Printf.sprintf "[Id.Gen]s are isolated up to n=%d" n)
      (if n > 10000 then `Slow else `Quick)
      (id_gens_isolated_test n)
  in
  ( "lib/id.ml",
    [
      create_id_gen_isolation_test 10;
      create_id_gen_isolation_test 100;
      create_id_gen_isolation_test 1000;
    ] )

let util_module_test_suite =
  let open Alcotest in
  let open Cs3110_compiler in
  let test_merge_paths =
    let test () =
      (check string) "An empty list yields the empty path." ""
        (Util.merge_paths []);
      (check string)
        "A singleton list of an empty string yields the empty path." ""
        (Util.merge_paths [ "" ]);
      (check string) "A list of empty strings yields the empty path." ""
        (Util.merge_paths [ ""; ""; "" ]);
      (check string) "Slashes are preserved at the front." "/"
        (Util.merge_paths [ "/" ]);
      (check string) "Slashes are preserved at the front." "/a"
        (Util.merge_paths [ "/a" ]);
      (check string) "Slashes are preserved at the front." "/a/b"
        (Util.merge_paths [ "/a"; "b" ]);
      (check string)
        "Path components should be trimmed of internal slashes and a single \
         slash inserted between"
        "a/b/c/d/e/f"
        (Util.merge_paths [ "a"; "/b"; "c/"; "/d/"; "e"; "f" ])
    in
    test_case "Util.merge_paths" `Quick test
  in
  ("lib/util.ml", [ test_merge_paths ])

let () =
  [
    util_module_test_suite;
    id_module_test_suite;
    Snapshot.make_test_suite "test/snapshots" ir_transform;
  ]
  |> Alcotest.run "x86ISTMB"
