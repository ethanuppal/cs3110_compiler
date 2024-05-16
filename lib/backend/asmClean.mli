(** [clean section] minimizes redundant assembly in [section] using naive rules
    that are guaranteed to not affect the data. *)
val clean : Asm.Section.t -> unit
