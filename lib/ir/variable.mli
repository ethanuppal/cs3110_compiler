(** The type of an IR variable. *)
type t

(** [make ()] is a new variable with a unique id. *)
val make : unit -> t

(** [id_of var] is the unique id of [var]. *)
val id_of : t -> int

(** [to_string var] is [var] as a string. *)
val to_string : t -> string
