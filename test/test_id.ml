let test_suite =
  let open Alcotest in
  let id_gens_isolated_test n () =
    let open X86ISTMB in
    let gen1 = Id.Gen.make () in
    let gen2 = Id.Gen.make () in
    (check @@ neg int)
      "Generators should have *different* self-ids. For some reason, Alcotest \
       doesn't display this differently than normal (issue #300)."
      (Id.Gen.id_of gen1 |> Id.int_of)
      (Id.Gen.id_of gen2 |> Id.int_of);
    let last = Id.Gen.next gen1 in
    (check int) "Using one generator should not affect the other."
      (Id.int_of last)
      (Id.Gen.next gen2 |> Id.int_of);
    for _ = 1 to n do
      (check int) "Using generators in parallel should yield equality of ids."
        (Id.Gen.next gen1 |> Id.int_of)
        (Id.Gen.next gen2 |> Id.int_of)
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
