module Variable : sig
  (** The type of an IR variable. *)
  type t

  (** [to_string var] is [var] as a string. *)
  val to_string : t -> string
end = struct
  open Util

  type t = int

  let to_string = string_of_int >> ( ^ ) "i"
end

type constant = int

(* todo *)
type t =
  | Add of Variable.t * Variable.t
  | IAdd of Variable.t * constant
  | Store of Variable.t * Variable.t
  | IStore of Variable.t * constant
  | Load of Variable.t * Variable.t
  | ILoad of Variable.t * constant
  | Param of Variable.t
  | IParam of constant
  | Jump of Basic_block.id
  | Call of Basic_block.id
