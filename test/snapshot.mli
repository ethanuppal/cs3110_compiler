open Alcotest

(** Let [f] be of type [transform]. Then, [f path contents] is the result of
    transforming the string [contents] (from the file at path [path]). *)
type transform = string -> string -> string

(** [make_test_suite name f] is a snapshot test suite named [name] for version
    [Meta.get.version] using snapshot transformation [f] (see the documentation
    for [transform]). *)
val make_test_suite : string -> transform -> unit test
