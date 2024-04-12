type constant = int

type t =
  | Variable of Variable.t
  | Constant of constant

let make_var x = Variable (Variable.make x)
let make_const x = Constant x

let to_string = function
  | Variable var -> Variable.to_string var
  | Constant const -> string_of_int const
