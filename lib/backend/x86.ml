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

  let to_string = function
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
end

module Operand = struct
  type t =
    | Register of Register.t
    | Intermediate of int
    | Label of string
    | RelativeLabel of string

  let to_string = function
    | Register reg -> Register.to_string reg
    | Intermediate int -> string_of_int int
    | Label label -> label
    | RelativeLabel rel_label -> "[rel " ^ rel_label ^ "]"
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
    | Ret
    | Syscall

  let to_string = function
    | Mov (op1, op2) ->
        "mov " ^ Operand.to_string op1 ^ ", " ^ Operand.to_string op2
    | Add (op1, op2) ->
        "add " ^ Operand.to_string op1 ^ ", " ^ Operand.to_string op2
    | Sub (op1, op2) ->
        "sub " ^ Operand.to_string op1 ^ ", " ^ Operand.to_string op2
    | IMul op -> "imul " ^ Operand.to_string op
    | Push op -> "push " ^ Operand.to_string op
    | Pop op -> "pop " ^ Operand.to_string op
    | Call op -> "call " ^ Operand.to_string op
    | Cmp (op1, op2) ->
        "cmp " ^ Operand.to_string op1 ^ ", " ^ Operand.to_string op2
    | Jmp op -> "jmp " ^ Operand.to_string op
    | Je op -> "je " ^ Operand.to_string op
    | Ret -> "ret"
    | Syscall -> "syscall"
end

module Section : sig
  type t

  (** `make name align` is new section with name [name] and alignment [align]. *)
  val make : string -> int -> t

  val to_string : t -> string
end = struct
  type t = {
    name : string;
    align : int;
    contents : Instruction.t BatDynArray.t;
  }

  let make name align = { name; align; contents = BatDynArray.make 16 }

  let to_string section =
    let open Util in
    ("section ." ^ section.name)
    :: (display_indent ^ "align " ^ string_of_int section.align)
    :: (BatDynArray.to_list section.contents
       |> List.map (Instruction.to_string >> ( ^ ) display_indent))
    |> String.concat "\n"
end

module AssemblyFile = struct end
