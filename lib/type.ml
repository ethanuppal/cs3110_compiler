module Primitive = struct
  (** [t] represents primitive type. *)
  type t =
    | Int63
    | Bool

  (** [to_string prim_type] is the string representation of [prim_type]. *)
  let to_string = function
    | Int63 -> "Int"
    | Bool -> "Bool"
end

type stmt_type =
  | Terminal
  | Nonterminal

(** [t] represents a type. *)
type t =
  | Primitive of Primitive.t
  | Pointer of t
  | Var of string
  | Any

(** [to_string ty] is the string representation of [ty]. *)
let rec to_string = function
  | Primitive prim -> Primitive.to_string prim
  | Pointer ty -> to_string ty ^ "*"
  | Var tvar -> tvar
  | Any -> "?"

let int_prim_type = Primitive Int63
let bool_prim_type = Primitive Bool
let any_type = Any

(** [deref ty] is [ty'] where [ty = Pointer ty'] for some [ty'].

    Requires: [ty] is of the above form. *)
let deref = function
  | Pointer ty -> ty
  | _ -> failwith "precondition"
