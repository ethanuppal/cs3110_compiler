open X86ISTMB

let print_help prog =
  let open Printf in
  printf "%s\n" Meta.get.description;
  printf "\n";
  printf "Usage: %s [-h|-v|-f FILE]\n" prog;
  printf "\n";
  printf "-h,--help       prints this info\n";
  printf "-v,--version    prints version info\n";
  printf "-f FILE         runs the main program on FILE\n"

let print_version () =
  let open Printf in
  printf "%s %s\n" Meta.get.name (Meta.Version.to_string Meta.get.version);
  printf "\n";
  printf "Written by: %s\n" (String.concat ", " Meta.get.authors)

let read_file filename = BatFile.lines_of filename |> BatList.of_enum |> List.map (fun e -> e ^ "\n") |> String.concat ""

let driver path =
  let source = read_file path in
  Cs3110_compiler.Parse_lex.lex_and_parse source

let () =
  match Sys.argv |> Tui.parse with
  | Help { prog } -> print_help prog
  | Version { prog = _ } -> print_version ()
  | File { prog = _; path } -> driver path
  | Error { prog; msg } -> Printf.printf "error: %s\nuse %s -h\n" msg prog
