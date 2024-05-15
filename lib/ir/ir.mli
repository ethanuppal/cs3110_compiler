(** The intermediate representation for x86istmb. *)
type t =
  | Assign of Variable.t * Operand.t
  | Add of Variable.t * Operand.t * Operand.t
  | Sub of Variable.t * Operand.t * Operand.t
  | Ref of Variable.t * Operand.t
  | Deref of Variable.t * Operand.t
  | TestEqual of Variable.t * Operand.t * Operand.t
  | DebugPrint of Operand.t
  | Call of Variable.t * string list * Operand.t list
  | Return of Operand.t option

(** [kill_of ir] is [Some var] if [var] is assigned to in [ir] and [None]
    otherwise. *)
val kill_of : t -> Variable.t option

(** [to_string ir] is a string representation of the IRk instruction [ir]. *)
val to_string : t -> string
