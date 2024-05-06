(** Let [f] be of type [transform]. Then, [f contents] is the result of
    transforming the string [contents] (which will usually come from a snapshot
    file) *)
type transform = string -> string

(** [make_test_suite root suite f] is a snapshot test suit from snapshots in
    [root/suite] using snapshot transformation [f] (see the documentation for
    [transform]). *)
val make_test_suite : string -> string -> transform -> unit Alcotest.test
