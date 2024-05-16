type t =
  | Assign of Variable.t * Operand.t
  | Add of Variable.t * Operand.t * Operand.t
  | Sub of Variable.t * Operand.t * Operand.t
  | Mul of Variable.t * Operand.t * Operand.t
  | Shl of Variable.t * Operand.t * Operand.t
  | Shr of Variable.t * Operand.t * Operand.t
  | Sar of Variable.t * Operand.t * Operand.t
  | Ref of Variable.t * Operand.t
  | Deref of Variable.t * Operand.t
  | TestEqual of Variable.t * Operand.t * Operand.t
  | DebugPrint of Operand.t
  | Call of Variable.t * string list * Operand.t list
  | GetParam of Variable.t
  | Return of Operand.t option

let kill_of = function
  | Assign (var, _)
  | Add (var, _, _)
  | Sub (var, _, _)
  | Mul (var, _, _)
  | Shl (var, _, _)
  | Shr (var, _, _)
  | Sar (var, _, _)
  | Ref (var, _)
  | Deref (var, _)
  | TestEqual (var, _, _)
  | GetParam var
  | Call (var, _, _) -> Some var
  | DebugPrint _ | Return _ -> None

let gen_of = function
  | Assign (_, op)
  | Ref (_, op)
  | Deref (_, op)
  | DebugPrint op
  | Return (Some op) -> [ op ]
  | Add (_, op1, op2)
  | Sub (_, op1, op2)
  | Mul (_, op1, op2)
  | Shl (_, op1, op2)
  | Shr (_, op1, op2)
  | Sar (_, op1, op2)
  | TestEqual (_, op1, op2) -> [ op1; op2 ]
  | Call (_, _, ops) -> ops
  | GetParam _ | Return None -> []

let to_string =
  let open Printf in
  function
  | Assign (r, o) ->
      sprintf "%s = %s" (Variable.to_string r) (Operand.to_string o)
  | Add (r, o1, o2) ->
      sprintf "%s = %s + %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Sub (r, o1, o2) ->
      sprintf "%s = %s - %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Mul (r, o1, o2) ->
      sprintf "%s = %s * %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Shl (r, o1, o2) ->
      sprintf "%s = %s << %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Shr (r, o1, o2) ->
      sprintf "%s = %s >> %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Sar (r, o1, o2) ->
      sprintf "%s = %s >> %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | Ref (r, o) ->
      sprintf "%s = &%s" (Variable.to_string r) (Operand.to_string o)
  | Deref (r, o) ->
      sprintf "%s = *%s" (Variable.to_string r) (Operand.to_string o)
  | TestEqual (r, o1, o2) ->
      sprintf "%s = %s == %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | DebugPrint op -> sprintf "std::debug_print(%s)" (Operand.to_string op)
  | Call (r, name, args) ->
      sprintf "%s = %s(%s)" (Variable.to_string r)
        (name |> String.concat "::")
        (args |> List.map Operand.to_string |> String.concat ", ")
  | GetParam var -> sprintf "%s = <next parameter>" (Variable.to_string var)
  | Return op ->
      sprintf "return%s"
        (match op with
        | Some op -> " " ^ Operand.to_string op
        | None -> "")
