(** Represents the operand of an IR instruction. *)
type t =
  | Variable of Variable.t
  | Constant of int
  | StringLiteral of StringLiteral.t

(** [make_var var] is a variable operand representing the variable [var]. *)
val make_var : Variable.t -> t

(** [make_const const] is a constant operand representing the constant [const]. *)
val make_const : int -> t

(** [make_string_literal value] is a constant operand representing a reference
    to the location of the string literal [value] in memory. *)
val make_string_literal : string -> t

(** [to_string operand] is the string representation of [operand]. *)
val to_string : t -> string

(** [var_of_opt operand] is [Some var] if [operand] is a variable operand, and
    [None] otherwise. *)
val var_of_opt : t -> Variable.t option
