open AstType

(** [type_of_expr expr] is the type of [expr] to the extent that it is currently
    resolved. *)
val type_of_expr : expr -> Type.t option

(** [expr_is_const expr] if and only if [expr] is a constant (i.e., cannot have
    an address taken of it). *)
val expr_is_const : expr -> bool

(** [op_to_string op] is a string representation of [op]. *)
val op_to_string : op -> string

(** [expr_to_string expr] is a string representation of [expr]. *)
val expr_to_string : expr -> string

(** [stmt_to_string stmt] is a string representation of [stmt]. *)
val stmt_to_string : stmt -> string

(** [pp_op fmt op] pretty prints [op] to [fmt] in the same manner as
    [op_to_string]. *)
val pp_op : Format.formatter -> op -> unit

(** [pp_expr fmt expr] pretty prints [expr] to [fmt] in the same manner as
    [expr_to_string]. *)
val pp_expr : Format.formatter -> expr -> unit

(** [pp_stmt fmt stmt] pretty prints [stmt] to [fmt] in the same manner as
    [stmt_to_string]. *)
val pp_stmt : Format.formatter -> stmt -> unit

(** [pp_prog fmt prog] pretty prints an entire program [prog] to [fmt]. *)
val pp_prog : Format.formatter -> stmt list -> unit
