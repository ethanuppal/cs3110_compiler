type constant = int

module VariableMap = Hashtbl.Make (Variable)

(** The kabIR for x86istmb. *)
type t =
  | Assign of Variable.t * Operand.t
  | Add of Variable.t * Operand.t * Operand.t
  | Sub of Variable.t * Operand.t * Operand.t
  | Ref of Variable.t * Operand.t
  | Deref of Variable.t * Operand.t
  | TestEqual of Variable.t * Operand.t * Operand.t
  | DebugPrint of Operand.t
  | Call of Variable.t * string list * Operand.t list
  | Return of Operand.t option

(** [kill_of ir] is [Some var] if [var] is assigned to in [ir] and [None]
    otherwise. *)
let kill_of = function
  | Assign (var, _)
  | Add (var, _, _)
  | Sub (var, _, _)
  | Ref (var, _)
  | Deref (var, _)
  | TestEqual (var, _, _) -> Some var
  | Call (var, _, _) -> Some var
  | DebugPrint _ | Return _ -> None

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
  | Ref (r, o) ->
      sprintf "%s = &%s" (Variable.to_string r) (Operand.to_string o)
  | Deref (r, o) ->
      sprintf "%s = *%s" (Variable.to_string r) (Operand.to_string o)
  | TestEqual (r, o1, o2) ->
      sprintf "%s = %s == %s" (Variable.to_string r) (Operand.to_string o1)
        (Operand.to_string o2)
  | DebugPrint op -> sprintf "debug_print %s" (Operand.to_string op)
  | Call (r, name, args) ->
      sprintf "%s = %s(%s)" (Variable.to_string r)
        (name |> String.concat "::")
        (args |> List.map Operand.to_string |> String.concat ",")
  | Return op ->
      sprintf "return%s"
        (match op with
        | Some op -> " " ^ Operand.to_string op
        | None -> "")
