module Primitive = struct
  (** [t] represents a primitive type. *)
  type t =
    | Int
    | Bool
    | Unit

  (** [to_string prim_type] is the string representation of [prim_type]. *)
  let to_string = function
    | Int -> "Int"
    | Bool -> "Bool"
    | Unit -> "Unit"
end

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
let rec to_string = function
  | Prim prim -> Primitive.to_string prim
  | Pointer ty -> to_string ty ^ "*"
  | Var tvar -> tvar
  | FunctionType { params; return } ->
      "("
      ^ (params |> List.map to_string |> String.concat ", ")
      ^ ") -> " ^ to_string return

let int_prim_type = Prim Int
let bool_prim_type = Prim Bool
let unit_prim_type = Prim Unit

(** [deref ty] is [Some ty'] if [ty = Pointer ty'] for some [ty'] and [None]
    otherwise. *)
let deref = function
  | Pointer ty' -> Some ty'
  | _ -> None
