(** A value in the interpreter. *)
type t =
  | Int of int
  | FunctionValue of { body : Ast.stmt list }

exception
  TypeError of {
    value : t;
    ctx : string;
  }

(** [to_string value] is the string representation of [value]. *)
let to_string = function
  | Int i -> string_of_int i
  | FunctionValue _ -> "<func>"

let as_int = function
  | Int i -> i
  | value -> raise (TypeError { value; ctx = "as_int" })

let as_func = function
  | FunctionValue { body } -> body
  | value -> raise (TypeError { value; ctx = "as_func" })
