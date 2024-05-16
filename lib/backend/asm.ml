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

  let compare = Stdlib.compare

  (** Caller saved registers. rsp is excluded because it is used to manage the
      stack and usually needs special care. *)
  let caller_saved_data_registers =
    [ RAX; RCX; RDX; RDI; RSI; R8; R9; R10; R11 ]

  (** Caller saved registers. rbp is excluded because it is used to manage the
      stack and usually needs special care. *)
  let callee_saved_data_registers = [ RBX; R12; R13; R14; R15 ]

  let data_registers = callee_saved_data_registers @ caller_saved_data_registers
  let parameter_registers = [ RDI; RSI; RDX; RCX; R8; R9 ]
end

module Operand = struct
  type t =
    | Register of Register.t
    | Deref of Register.t * int
    | Intermediate of int
    | Label of string
    | RelativeLabel of string

  let to_nasm = function
    | Register reg -> Register.to_nasm reg
    | Deref (reg, off) -> Printf.sprintf "[%s + %d]" (Register.to_nasm reg) off
    | Intermediate int -> string_of_int int
    | Label label -> label
    | RelativeLabel rel_label -> "[rel " ^ rel_label ^ "]"
end

module Label = struct
  type t = {
    is_global : bool;
    is_external : bool;
    name : string;
  }

  let make ~is_global ~is_external name = { is_global; is_external; name }
  let name_of label = label.name

  let to_nasm label =
    match (label.is_global, label.is_external) with
    | false, false -> label.name ^ ":"
    | true, false -> "global " ^ label.name ^ "\n" ^ label.name ^ ":"
    | false, true -> "extern " ^ label.name
    | _ -> failwith "invalid label"
end

module Instruction = struct
  type t =
    | Mov of Operand.t * Operand.t
    | Lea of Operand.t * Operand.t
    | Add of Operand.t * Operand.t
    | Sub of Operand.t * Operand.t
    | IMul of Operand.t * Operand.t
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
    | DataBytes of int list
    | Shl of Operand.t * Operand.t
    | Shr of Operand.t * Operand.t
    | Sar of Operand.t * Operand.t

  let to_nasm = function
    | Mov (op1, op2) ->
        "mov qword " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | Lea (op1, op2) ->
        "lea " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | Add (op1, op2) ->
        "add " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | Sub (op1, op2) ->
        "sub " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | IMul (op1, op2) ->
        "imul " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
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
    | DataBytes data ->
        "db " ^ (data |> List.map string_of_int |> String.concat ", ")
    | Shl (op1, op2) ->
        "shl " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | Shr (op1, op2) ->
        "shr " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
    | Sar (op1, op2) ->
        "sar " ^ Operand.to_nasm op1 ^ ", " ^ Operand.to_nasm op2
end

module Section = struct
  type t = {
    name : string;
    align : int;
    contents : Instruction.t BatDynArray.t;
  }

  let make name align = { name; align; contents = BatDynArray.make 16 }
  let add section = BatDynArray.add section.contents
  let add_all section instrs = List.iter (add section) instrs

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

  let get_instr section idx = BatDynArray.get section.contents idx
  let set_instr section idx instr = BatDynArray.set section.contents idx instr
  let rem_instr section idx = BatDynArray.delete section.contents idx
  let length_of section = BatDynArray.length section.contents
end

module AssemblyFile = struct
  type t = Section.t BatDynArray.t

  let make () = BatDynArray.make 16
  let add = BatDynArray.add

  let to_nasm =
    BatDynArray.to_list >> List.map Section.to_nasm >> String.concat "\n\n"
end
