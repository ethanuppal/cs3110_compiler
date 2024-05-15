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
let caller_saved_data_registers = [ RAX; RCX; RDX; RDI; RSI; R8; R9; R10; R11 ]
let callee_saved_data_registers = [ RBX; R12; R13; R14; R15 ]
let data_registers = caller_saved_data_registers @ callee_saved_data_registers
let parameter_registers = [ RDI; RSI; RDX; RCX; R8; R9 ]
