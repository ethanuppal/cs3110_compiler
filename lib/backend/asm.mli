(** Contains functionality for dealing with x86-64 registers. *)
module Register : sig
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

  (** [to_nasm reg] is the NASM representation of [reg]. *)
  val to_nasm : t -> string

  (** Provides an arbitrary ordering of registers. *)
  val compare : t -> t -> int

  (** Caller saved registers. rsp is excluded because it is used to manage the
      stack and usually needs special care. *)
  val caller_saved_data_registers : t list

  (** Caller saved registers. rbp is excluded because it is used to manage the
      stack and usually needs special care. *)
  val callee_saved_data_registers : t list

  (** All register that can be used to store general data. *)
  val data_registers : t list

  (** Register that can be used to pass parameters, in order of first parameter
      to last parameter according to the System V ABI. *)
  val parameter_registers : t list
end

(** Contains functionality for dealing with x86-64 operands. *)
module Operand : sig
  (** Represents an operand to an instruction. *)
  type t =
    | Register of Register.t
    | Deref of Register.t * int
    | Intermediate of int
    | Label of string
    | RelativeLabel of string

  (** [to_nasm op] is the NASM representation of [op]. *)
  val to_nasm : t -> string
end

(** Contains functionality for dealing with NASM block labels. *)
module Label : sig
  type t

  (** [make ~is_global:is_global, ~is_external:is_external name] is a label
      named [name], global if and only if [is_global], and external if and only
      if [is_external], but not both. *)
  val make : is_global:bool -> is_external:bool -> string -> t

  (** [to_nasm label] is the NASM representation of [label]. *)
  val to_nasm : t -> string
end

(** Contains functionality for dealing with x86-64 instructions. *)
module Instruction : sig
  (** Represents and x86-64 instruction. *)
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

  (** [to_nasm instr] is the NASM representation of [instr]. *)
  val to_nasm : t -> string
end

(** Contains functionality for creating assembly sections. *)
module Section : sig
  (** Values of type [t] are assembly sections. *)
  type t

  (** `make name align` is new section with name [name] and alignment [align]. *)
  val make : string -> int -> t

  (** [add section instr] adds [instr] to the end of [section]. *)
  val add : t -> Instruction.t -> unit

  (** [add_all section instructions] adds all instructions in [instructions] to
      [section] in order. It is equivalent to calling
      [List.iter (Section.add section) instructions]*)
  val add_all : t -> Instruction.t list -> unit

  (** [to_nasm section] is the NASM representation of [section]. *)
  val to_nasm : t -> string
end

(** Contains functionality for creating assembly files. *)
module AssemblyFile : sig
  (** Represents an assembly file containing many assembly sections. *)
  type t

  (** [make ()] is an empty assembly file with no sections. *)
  val make : unit -> t

  (** [add file section] adds [section] to [file]. *)
  val add : t -> Section.t -> unit

  (** [to_nasm file] is the NASM code for [file]. *)
  val to_nasm : t -> string
end
