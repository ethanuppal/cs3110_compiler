(** [emit section cfg] emits the function [cfg] into the assembly section
    [section]. *)
val emit : Asm.Section.t -> Cfg.t -> unit
