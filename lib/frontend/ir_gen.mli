(** [generate ast] is a list of control flow graphs, where each control flow
    graph represents one top-level function. *)
val generate : AstType.prog -> Cfg.t list
