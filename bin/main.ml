open X86ISTMB

let print_error = Printf.eprintf "error: %s"

let print_help prog =
  let open Printf in
  printf "%s\n" Meta.get.description;
  printf "\n";
  printf "Usage: %s [-h|-v]\n" prog;
  printf "   or: %s FILE [-g][-O]\n" prog;
  printf "\n";
  printf "-h,--help         prints this info\n";
  printf "-v,--version      prints version info\n";
  printf "-g,--gen          only produces IR\n";
  printf "-O,--optimize     runs optimizations\n";
  ignore ()

let print_version () =
  let open Printf in
  printf "%s %s\n" Meta.get.name (Meta.Version.to_string Meta.get.version);
  printf "\n";
  printf "Written by: %s\n" (String.concat ", " Meta.get.authors)

let rec dispatch action prog =
  match action with
  | Cli.Help -> print_help prog
  | Version -> print_version ()
  | Compile { paths; flags } ->
      if List.is_empty paths then
        dispatch (Error { msg = "expected at least one file name" }) prog
      else Driver.compile paths flags None
  | Error { msg } -> Printf.sprintf "%s\nuse %s -h\n" msg prog |> print_error

let main args =
  let parse = Cli.parse args in
  dispatch parse.action parse.prog

let () = main Sys.argv
