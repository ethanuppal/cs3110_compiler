(** The type of an IR variable. *)
type t

(** [make ()] is a new variable with a unique id. *)
val make : unit -> t

(** [id_of var] is the unique id of [var]. *)
val id_of : t -> Id.t

(** [to_string var] is [var] as a string. *)
val to_string : t -> string

(** [pp fmt var] pretty prints [var] to [fmt]. *)
val pp : Format.formatter -> t -> unit

(** Provides an arbitrary ordering of variables. *)
val compare : t -> t -> int

(** [equal var1 var2] is true when [id_of var1 = id_of var2], and false
    otherwise. *)
val equal : t -> t -> bool

(** [hash var] is the hashcode of [var]. *)
val hash : t -> int
