(** Represents an x86-64 register. *)
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

(** [to_nasm reg] is the NASM language representation of [reg]. *)
val to_nasm : t -> string

(** Provides an arbitrary ordering of all registers. *)
val compare : t -> t -> int

(** Caller saved registers. rsp is excluded because it is used to manage the
    stack and usually needs special care. *)
val caller_saved_data_registers : t list

(** Caller saved registers. rbp is excluded because it is used to manage the
    stack and usually needs special care. *)
val callee_saved_data_registers : t list

(** All registers that can be used for general data. *)
val data_registers : t list

(** Registers that can be used for parameter passing, in order of first to last
    parameter based on the System V ABI. *)
val parameter_registers : t list
