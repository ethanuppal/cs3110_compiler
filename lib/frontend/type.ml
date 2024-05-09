module Primitive = struct
  (** [t] represents a primitive type. *)
  type t =
    | Int
    | Bool

  (** [to_string prim_type] is the string representation of [prim_type]. *)
  let to_string = function
    | Int -> "Int"
    | Bool -> "Bool"
end

type stmt_type =
  | Terminal
  | Nonterminal

(** [t] represents a type. *)
type t =
  | Prim of Primitive.t
  | Pointer of t
  | Var of string

(** [to_string ty] is the string representation of [ty]. *)
let rec to_string = function
  | Prim prim -> Primitive.to_string prim
  | Pointer ty -> to_string ty ^ "*"
  | Var tvar -> tvar

let int_prim_type = Prim Int
let bool_prim_type = Prim Bool

(** [deref ty] is [Some ty'] if [ty = Pointer ty'] for some [ty'] and [None]
    otherwise. *)
let deref = function
  | Pointer ty' -> Some ty'
  | _ -> None
