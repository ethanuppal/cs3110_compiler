type t =
  | Variable of Variable.t
  | Constant of int
  | StringLiteral of StringLiteral.t

let make_var var = Variable var
let make_const x = Constant x
let make_string_literal value = StringLiteral (StringLiteral.make value)

let to_string = function
  | Variable var -> Variable.to_string var
  | Constant const -> string_of_int const
  | StringLiteral value -> StringLiteral.to_string value

let var_of_opt = function
  | Variable var -> Some var
  | Constant _ | StringLiteral _ -> None
