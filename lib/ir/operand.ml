type t =
  | Variable of Variable.t
  | Constant of int

let make_var var = Variable var
let make_const x = Constant x

let to_string = function
  | Variable var -> Variable.to_string var
  | Constant const -> string_of_int const

let var_of_opt = function
  | Variable var -> Some var
  | Constant _ -> None
