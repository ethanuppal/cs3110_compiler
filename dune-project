(lang dune 3.14)

(name cs3110_compiler)

(using menhir 3.0)

(generate_opam_files false)

(source
 (github ethanuppal/cs3110_compiler))

(authors
 "Utku Melemeti"
 "Jason Klein"
 "Utku Melemetci"
 "Jeffrey Huang"
 "Vijay Shanmugam"
 "Ethan Uppal")

(maintainers "CS 3110 Compiler Team")

(documentation https://github.com/ethanuppal/cs3110_compiler/wiki)

(package
 (name cs3110_compiler)
 (synopsis "CS 3110 final project")
 (depends
  (ocaml
   (>= 5.1.1))
  ocaml
  dune
  menhir
  batteries
  (alcotest :with-test))
 (tags
  (cs3110 compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
