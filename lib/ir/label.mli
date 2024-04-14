open Id

(** A label. *)
type t

(** TODO: make work with multiple files and stuff *)

(** [make_symbol gen name] is a new label made using the id generator [gen] and
    named ["_" ^ name] suitable for use as a symbol.

    Requires: a label with name [name] must not already exist. *)
val make_symbol : ?is_external:bool -> ?is_global:bool -> Gen.t -> string -> t

(** [make_plain gen] is, if [gen ()] yields [id], a label with name
    [".L" ^ string_of_int (Id.Gen.id_of gen) ^ "_" ^ string_of_int id]. *)
val make_location : Gen.t -> t

(** [resolve name] is the label named [name].

    Requires: [name] has previously been bound. *)
val resolve : string -> t

(** [id_of label] is the identifier of [label]. *)
val id_of : t -> id

(** [name_of label] is the name of [label]. *)
val name_of : t -> string

(** [is_external label] if and only if [label] is an external symbol, i.e.,
    found in other source or compiled files. *)
val is_external : t -> bool

(** [is_global label] if and only if [label] is a global symbol, i.e., should be
    exposed outside of the compilation unit. *)
val is_global : t -> bool
