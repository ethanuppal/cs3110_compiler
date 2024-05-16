(** Module for primitive types. *)
module Primitive : sig
  (** [t] represents a primitive type. *)
  type t =
    | Int
    | Char
    | Bool
    | Unit

  (** [to_string prim_type] is the string representation of [prim_type]. *)
  val to_string : t -> string
end

(** Represents whether a statement is terminal or nonterminal. *)
type stmt_type =
  | Terminal
  | Nonterminal

(** [t] represents a type. *)
type t =
  | Prim of Primitive.t
  | Pointer of t
  | Var of string
  | FunctionType of {
      params : t list;
      return : t;
    }

(** [to_string ty] is the string representation of [ty]. *)
val to_string : t -> string

(** A type representing an integer. *)
val int_prim_type : t

(** A type representing a boolean. *)
val bool_prim_type : t

(** A type representing a byte. *)
val char_prim_type : t

(** A type representing a unit. *)
val unit_prim_type : t

(** [deref ty] is [Some ty'] if [ty = Pointer ty'] for some [ty'] and [None]
    otherwise. *)
val deref : t -> t option

(** [equal t1 t2] if and only if [t1] and [t2] are the same type. *)
val equal : t -> t -> bool
