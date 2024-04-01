open X86ISTMB

let print_error = Printf.eprintf "error: %s"

let print_help prog =
  let open Printf in
  printf "%s\n" Meta.get.description;
  printf "\n";
  printf "Usage: %s [-h|-v]\n" prog;
  printf "   or: %s -f FILE [-i|-c]\n" prog;
  printf "   or: %s -r\n" prog;
  printf "\n";
  printf "-h,--help         prints this info\n";
  printf "-v,--version      prints version info\n";
  printf "-f,--file FILE    uses the contents of FILE\n";
  printf "-i                runs the interpreter (default)\n";
  printf "-c                runs the compiler\n";
  printf "-r,--repl         starts the REPL\n"

let print_version () =
  let open Printf in
  printf "%s %s\n" Meta.get.name (Meta.Version.to_string Meta.get.version);
  printf "\n";
  printf "Written by: %s\n" (String.concat ", " Meta.get.authors)

let file_driver path flag =
  let source = Util.read_file path in
  try
    let statements = Frontend.ParseLex.lex_and_parse source in
    if flag = Cli.UseInterpreter then
      let interpreter = Interpreter.create () in
      List.iter interpreter.step statements
    else failwith "compiler not done yet"
  with Frontend.ParseLex.ParseError msg -> print_error (msg ^ "\n")

let repl_driver () =
  let interpreter = Interpreter.create () in
  interpreter.set_mode REPL;
  print_endline "[Welcome to the repl]";
  print_endline "[Type '#quit' to exit]";
  print_endline "[Type '#dump' for a debug dump]";
  let rec repl () =
    print_string "> ";
    flush stdout;
    let line = read_line () ^ "\n" in
    match line with
    | "#quit\n" -> ()
    | "#dump\n" ->
        interpreter.dump ();
        repl ()
    | _ -> (
        try
          let statements = Frontend.ParseLex.lex_and_parse line in
          List.iter interpreter.step statements;
          repl ()
        with Frontend.ParseLex.ParseError msg -> print_error (msg ^ "\n"))
  in
  repl ()

let () =
  match Sys.argv |> Cli.parse with
  | Help { prog } -> print_help prog
  | Version { prog = _ } -> print_version ()
  | File { prog = _; path; flag } -> file_driver path flag
  | Repl { prog = _ } -> repl_driver ()
  | Error { prog; msg } ->
      Printf.sprintf "%s\nuse %s -h\n" msg prog |> print_error
