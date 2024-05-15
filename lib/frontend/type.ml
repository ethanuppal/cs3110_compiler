module Primitive = struct
  type t =
    | Int
    | Bool
    | Unit

  let to_string = function
    | Int -> "Int"
    | Bool -> "Bool"
    | Unit -> "Unit"
end

type stmt_type =
  | Terminal
  | Nonterminal

type t =
  | Prim of Primitive.t
  | Pointer of t
  | Var of string
  | FunctionType of {
      params : t list;
      return : t;
    }

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

let deref = function
  | Pointer ty' -> Some ty'
  | _ -> None
