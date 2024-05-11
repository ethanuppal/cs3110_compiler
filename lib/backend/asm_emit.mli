(** [emit_preamble ~text:text] emits the x86istmb runtime preamble into the
    assembly code section [text]. *)
val emit_preamble : text:Asm.Section.t -> unit

(** [emit ~text:text cfg regalloc] emits the function [cfg] with register
    allocation [regalloc] into the assembly code section [text]. *)
val emit_cfg :
  text:Asm.Section.t -> Cfg.t -> Regalloc.allocation Ir.VariableMap.t -> unit
