(* todo *)
type t =
  | Assign of Variable.t * Operand.t
  | Add of Variable.t * Operand.t * Operand.t
  | Store of Variable.t * Operand.t
  | Load of Variable.t * Operand.t
  | Param of Operand.t
  | Jump of Label.t
  | Call of Label.t

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
  | Param o -> sprintf "arg %s" (Operand.to_string o)
  | Jump label -> sprintf "jump %s" (Label.name_of label)
  | Call label -> sprintf "call %s" (Label.name_of label)
