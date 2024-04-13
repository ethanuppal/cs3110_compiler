let id_module_test_suite =
  let open Alcotest in
  let id_gens_isolated_test n () =
    let open X86ISTMB in
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
  let open X86ISTMB in
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

let cfg_module_test_suite =
  let open Alcotest in
  let open X86ISTMB in
  let test_1 =
    let test () =
      Id.Gen.hard_reset ();
      let cfg = Control_flow_graph.make () in
      let entry = Control_flow_graph.entry cfg in
      let symbol_gen = Id.Gen.make () in
      let ir3 =
        [|
          Ir.Jump (Basic_block.label_of entry, Ir.BranchCondition.Unconditional);
        |]
      in
      let ir2 =
        [|
          Ir.Param (Ir.const 5);
          Ir.Call (Label.make_symbol symbol_gen "print");
          Ir.Jump
            ( Control_flow_graph.label_for_new_bb cfg,
              Ir.BranchCondition.Unconditional );
        |]
      in
      let ir1 =
        [|
          Ir.Assign (Ir.var 0, Ir.const 4);
          Ir.Jump
            ( Control_flow_graph.label_for_new_bb cfg,
              Ir.BranchCondition.Equal (Ir.var_op 0, Ir.const 0) );
        |]
      in
      Control_flow_graph.insert_ir cfg entry ir1.(0) |> ignore;
      let bb2 = Control_flow_graph.insert_ir cfg entry ir1.(1) |> Option.get in
      Control_flow_graph.insert_ir cfg bb2 ir2.(0) |> ignore;
      Control_flow_graph.insert_ir cfg bb2 ir2.(1) |> ignore;
      let bb3 = Control_flow_graph.insert_ir cfg bb2 ir2.(2) |> Option.get in
      let entry_hopefully =
        Control_flow_graph.insert_ir cfg bb3 ir3.(0) |> Option.get
      in
      (check string) "Basic block backreference should work"
        (Basic_block.label_of entry |> Label.name_of)
        (Basic_block.label_of entry_hopefully |> Label.name_of);
      match Control_flow_graph.to_list cfg with
      | [ cfg_bb1; cfg_bb2; cfg_bb3 ] ->
          let check_match ir bb =
            (check (list string))
              "The IR maintained by the CFG should be the same as the order \
               basic block labels were requested (i.e., in this case, 1 3 2)."
              (ir |> Array.to_seq |> List.of_seq |> List.map Ir.to_string)
              (bb |> Basic_block.to_list |> List.map Ir.to_string)
          in
          check_match ir1 cfg_bb1;
          check_match ir3 cfg_bb2;
          check_match ir2 cfg_bb3
      | _ ->
          fail
            "Control_flow_graph.to_list did not return the two (no more, no \
             less) basic blocks created."
    in
    test_case "test1" `Quick test
  in
  ("lib/control_flow_graph.mli", [ test_1 ])

let snapshot_test_suite =
  let interpreter_transform path input =
    ignore path;
    let open X86ISTMB in
    let interpreter = Interpreter.create () in
    let stdout = ref "" in
    let statements = Parse_lex.lex_and_parse input in
    interpreter.set_mode (Text stdout);
    List.iter interpreter.step statements;
    stdout
  in
  let () = ignore interpreter_transform in
  let transform path input =
    let open X86ISTMB in
    let open Util in
    let stmt_to_string stmt =
      Ast.pp_stmt Format.str_formatter stmt;
      Format.flush_str_formatter ()
    in
    if String.starts_with ~prefix:"type" path then
      try
        let statements = Parse_lex.lex_and_parse input in
        Analysis.infer statements;
        List.map (stmt_to_string >> fun s -> s ^ "\n") statements
        |> String.concat ""
      with
      | Analysis.TypeInferenceError err ->
          Printexc.to_string (Analysis.TypeInferenceError err) ^ "\n"
      | e -> raise e
    else ""
  in
  Snapshot.make_test_suite "test/snapshots" transform

let () =
  [
    util_module_test_suite;
    id_module_test_suite;
    cfg_module_test_suite;
    snapshot_test_suite;
  ]
  |> Alcotest.run "x86ISTMB"
