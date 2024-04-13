(** Let [f] be of type [transform]. Then, [f path contents] is the result of
    transforming the string [contents] (from the file at path [path] relative to
    the version root, e.g., for version v1.0.4, relative to
    [test/snapshots/v1.0.4]). *)
type transform = string -> string -> string

(** [make_test_suite name f] is a snapshot test suite named [name] for version
    [Meta.get.version] using snapshot transformation [f] (see the documentation
    for [transform]). Optionally, the version can be manually specified. *)
val make_test_suite :
  ?version:Cs3110_compiler.Meta.Version.t ->
  string ->
  transform ->
  unit Alcotest.test
