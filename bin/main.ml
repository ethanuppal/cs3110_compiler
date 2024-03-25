open Cs3110_compiler

let print_help prog =
  let open Printf in
  printf "%s\n" Meta.get.description;
  printf "\n";
  printf "Usage: %s [-h|-v]\n" prog

let print_version () =
  let open Printf in
  printf "%s %s\n" Meta.get.name (Meta.Version.to_string Meta.get.version);
  printf "\n";
  printf "Written by: %s\n" (String.concat ", " Meta.get.authors)

let () =
  match Sys.argv |> Tui.parse with
  | Help { prog } -> print_help prog
  | Version { prog = _ } -> print_version ()
  | Error { prog; msg } -> Printf.printf "error: %s. use %s -h\n" msg prog
