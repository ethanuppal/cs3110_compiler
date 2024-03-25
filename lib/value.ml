(** A value in the interpreter. *)
type t = int (* todo have variant *)

(** [to_string value] is the string representation of [value]. *)
let to_string = string_of_int
