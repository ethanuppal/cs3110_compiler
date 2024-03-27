let () =
  let project_root = Sys.getenv "PWD" in
  let ocaml_code = Printf.sprintf "let path = \"%s\"" project_root in
  let ocaml_file = open_out "project_root.ml" in
  output_string ocaml_file ocaml_code;
  close_out ocaml_file
