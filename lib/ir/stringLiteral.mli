(** Values of type [t] are string literals. *)
type t

(** [make value] is a new string literal with contents [value]. *)
val make : string -> t

(** [value_of str_lit] is the string literal value of [str_lit]. *)
val value_of : t -> string

(** [label_for str_lit] is a label for [str_lit]. *)
val label_for : t -> string

(** [to_string str_lit] is a string representation of [str_lit]. *)
val to_string : t -> string
