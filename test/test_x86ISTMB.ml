let interpreter_transform path input =
  ignore path;
  let open X86ISTMB in
  let interpreter = Interpreter.create () in
  let stdout = ref "" in
  let statements = Frontend.ParseLex.lex_and_parse input in
  interpreter.set_mode (Text stdout);
  List.iter interpreter.step statements;
  stdout

let () = ignore interpreter_transform

let ir_transform path input =
  ignore path;
  ignore input;
  ""

let () =
  [ Snapshot.make_test_suite "test/snapshots" ir_transform ]
  |> Alcotest.run "x86ISTMB"
