(** [emit_preamble ~text_section ~data_section ffi_names decl_names] emits the
    x86istmb runtime preamble into the assembly code section [text_section] and
    data secetion [data_section], referencing foreign external symbols
    [ffi_names] and in-language external symbols [decl_names]. *)
val emit_preamble :
  text_section:Asm.Section.t ->
  data_section:Asm.Section.t ->
  string list ->
  string list list ->
  unit

(** [emit ~text:text cfg regalloc] emits the function [cfg] with register
    allocation [regalloc] into the assembly code section [text]. *)
val emit_cfg :
  text_section:Asm.Section.t ->
  data_section:Asm.Section.t ->
  Cfg.t ->
  Regalloc.allocation VariableMap.t ->
  unit
