open Util

let display_indent = "  "

module Register = struct
  type t =
    | RAX
    | RBX
    | RCX
    | RDX
    | RBP
    | RSP
    | RSI
    | RDI
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15

  let to_nasm = function
    | RAX -> "rax"
    | RBX -> "rbx"
    | RCX -> "rcx"
    | RDX -> "rdx"
    | RBP -> "rbp"
    | RSP -> "rsp"
    | RSI -> "rsi"
    | RDI -> "rdi"
    | R8 -> "r8"
    | R9 -> "r9"
    | R10 -> "r10"
    | R11 -> "r11"
    | R12 -> "r12"
    | R13 -> "r13"
    | R14 -> "r14"
    | R15 -> "r15"

  let all_registers =
    [
      RAX;
      RBX;
      RCX;
      RDX;
      RBP;
      RSP;
      RSI;
      RDI;
      R8;
      R9;
      R10;
      R11;
      R12;
      R13;
      R14;
      R15;
    ]

  let compare = Stdlib.compare
end

module Operand = struct
  type t =
    | Register of Register.t
    | Intermediate of int
    | Label of string
    | RelativeLabel of string

  let to_nasm = function
    | Register reg -> Register.to_nasm reg
    | Intermediate int -> string_of_int int
    | Label label -> label
    | RelativeLabel rel_label -> "[rel " ^ rel_label ^ "]"
end

module Label : sig
  type t

  (** [make ~is_global:is_global, ~is_external:is_external name] is a label
      named [name], global if and only if [is_global], and external if and only
      if [is_external], but not both. *)
  val make : is_global:bool -> is_external:bool -> string -> t

  val to_nasm : t -> string
end = struct
  type t = {
    is_global : bool;
    is_external : bool;
    name : string;
  }

  let make ~is_global ~is_external name = { is_global; is_external; name }

  let to_nasm label =
    match (label.is_global, label.is_external) with
    | false, false -> label.name
    | true, false -> "global " ^ label.name ^ "\n" ^ display_indent ^ label.name
    | false, true ->
        "external " ^ label.name ^ "\n" ^ display_indent ^ label.name
    | _ -> failwith "invalid label"
end

module Instruction = struct
  type t =
    | Mov of Operand.t * Operand.t
    | Add of Operand.t * Operand.t
    | Sub of Operand.t * Operand.t
    | IMul of Operand.t
    | Push of Operand.t
    | Pop of Operand.t
    | Call of Operand.t
    | Cmp of Operand.t * Operand.t
    | Jmp of Operand.t
    | Je of Operand.t
    | Jne of Operand.t
    | Ret
    | Syscall
    | Label of Label.t

  let to_nasm = function
    | Mov (op1, op2) ->
        "mov " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | Add (op1, op2) ->
        "add " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | Sub (op1, op2) ->
        "sub " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | IMul op -> "imul " ^ Operand.to_nasm op
    | Push op -> "push " ^ Operand.to_nasm op
    | Pop op -> "pop " ^ Operand.to_nasm op
    | Call op -> "call " ^ Operand.to_nasm op
    | Cmp (op1, op2) ->
        "cmp " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | Jmp op -> "jmp " ^ Operand.to_nasm op
    | Je op -> "je " ^ Operand.to_nasm op
    | Jne op -> "jne " ^ Operand.to_nasm op
    | Ret -> "ret"
    | Syscall -> "syscall"
    | Label label -> Label.to_nasm label
end

module Section : sig
  (** Values of type [t] are assembly sections. *)
  type t

  (** `make name align` is new section with name [name] and alignment [align]. *)
  val make : string -> int -> t

  (** [add section instr] adds [instr] to the end of [section]. *)
  val add : t -> Instruction.t -> unit

  val to_nasm : t -> string
end = struct
  type t = {
    name : string;
    align : int;
    contents : Instruction.t BatDynArray.t;
  }

  let make name align = { name; align; contents = BatDynArray.make 16 }
  let add section = BatDynArray.add section.contents

  let to_nasm section =
    ("section ." ^ section.name)
    :: (display_indent ^ "align " ^ string_of_int section.align)
    :: (BatDynArray.to_list section.contents
       |> List.map (fun instr ->
              let str = Instruction.to_nasm instr in
              match instr with
              | Label _ -> str
              | _ -> display_indent ^ str))
    |> String.concat "\n"
end

module AssemblyFile : sig
  type t

  val add : t -> Section.t -> unit
  val to_nasm : t -> string
end = struct
  type t = Section.t BatDynArray.t

  let add = BatDynArray.add

  let to_nasm =
    BatDynArray.to_list >> List.map Section.to_nasm >> String.concat "\n\n"
end
