module Variable : sig
  (** The type of an IR variable. *)
  type t

  val make : int -> t

  (** [to_string var] is [var] as a string. *)
  val to_string : t -> string
end = struct
  open Util

  type t = int

  let make x = x
  let to_string = string_of_int >> ( ^ ) "i"
end

module Operand = struct
  type t =
    | Variable of Variable.t
    | Constant of int

  let make_var x = Variable (Variable.make x)
  let make_const x = Constant x

  let to_string = function
    | Variable var -> Variable.to_string var
    | Constant const -> string_of_int const
end

type constant = int

module BranchCondition = struct
  type t =
    | Unconditional
    | Equal of Operand.t * Operand.t
    | Unequal of Operand.t * Operand.t
    | LessThan of Operand.t * Operand.t
    | GreaterThan of Operand.t * Operand.t
    | AtMost of Operand.t * Operand.t
    | AtLeast of Operand.t * Operand.t

  let to_string =
    let open Printf in
    function
    | Unconditional -> "true"
    | Equal (o1, o2) ->
        sprintf "%s == %s" (Operand.to_string o1) (Operand.to_string o2)
    | Unequal (o1, o2) ->
        sprintf "%s != %s" (Operand.to_string o1) (Operand.to_string o2)
    | LessThan (o1, o2) ->
        sprintf "%s < %s" (Operand.to_string o1) (Operand.to_string o2)
    | GreaterThan (o1, o2) ->
        sprintf "%s > %s" (Operand.to_string o1) (Operand.to_string o2)
    | AtMost (o1, o2) ->
        sprintf "%s <= %s" (Operand.to_string o1) (Operand.to_string o2)
    | AtLeast (o1, o2) ->
        sprintf "%s >= %s" (Operand.to_string o1) (Operand.to_string o2)
end

(* todo *)
type t =
  | Assign of Variable.t * Operand.t
  | Add of Variable.t * Operand.t * Operand.t
  | Store of Variable.t * Operand.t
  | Load of Variable.t * Operand.t
  | Param of Operand.t
  | Jump of Label.t * BranchCondition.t
  | Call of Label.t

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
  | Param o -> sprintf "arg %s" (Operand.to_string o)
  | Jump (label, br_cond) ->
      sprintf "jump %s if %s" (Label.name_of label)
        (BranchCondition.to_string br_cond)
  | Call label -> sprintf "call %s" (Label.name_of label)
