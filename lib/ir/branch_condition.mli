(** Represents a condition for branching. [Always] means the branch should
    always be taken, [Never] means the branch should never be taken, and
    [Conditional op] means the branch should be taken only if [op] is zero. *)
type t =
  | Always
  | Never
  | Conditional of Operand.t

(** [to_string cond] is a string representation of [cond]. *)
val to_string : t -> string
