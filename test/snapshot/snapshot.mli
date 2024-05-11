(** Let [(f, _)] be of type [transform]. Then, [f filename contents] is the
    result of transforming the string [contents] as contained in the snapshot
    file [filename] *)
type transform = (string -> string -> string) * Alcotest.speed_level

(** [make_test_suite root suite f] is a snapshot test suit from snapshots in
    [root/suite] using snapshot transformation [f] (see the documentation for
    [transform]). *)
val make_test_suite : string -> string -> transform -> unit Alcotest.test
