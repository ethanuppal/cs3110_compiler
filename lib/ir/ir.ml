type constant = int

(* todo *)
type t =
  | Assign of Variable.t * Operand.t
  | Add of Variable.t * Operand.t * Operand.t
  | Store of Variable.t * Operand.t
  | Load of Variable.t * Operand.t
  | Call of string * Operand.t list

let var = Variable.make
let var_op = Operand.make_var
let const = Operand.make_const

let to_string =
  let open Printf in
  function
  | Assign (r, o) ->
      sprintf "%s = %s" (Variable.to_string r) (Operand.to_string o)
  | Add (r, o1, o2) ->
      sprintf "%s = %s + %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Store (r, o) ->
      sprintf "*%s = %s" (Variable.to_string r) (Operand.to_string o)
  | Load (r, o) ->
      sprintf "%s = *%s" (Variable.to_string r) (Operand.to_string o)
  | Call (name, operands) ->
      sprintf "call %s %s" name
        (operands |> List.map Operand.to_string |> String.concat ", ")