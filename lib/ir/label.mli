open Id

(** A label. *)
type t

(** TODO: make work with multiple files and stuff *)

(** [make gen name] is a new label made using the id generator [gen] and named
    [name].

    Requires: A label with name [name] must not already exist. *)
val make : Gen.t -> string -> t

(** [make_plain gen] is, if [gen ()] yields [id], a label with name
    [".L" ^ (string_of_int id)]. *)
val make_plain : Gen.t -> t

(** [resolve name] is the label named [name].

    Requires: [name] has previously been bound. *)
val resolve : string -> t

(** [id_of label] is the identifier of [label]. *)
val id_of : t -> id

(** [name_of label] is the name of [label]. *)
val name_of : t -> string
