(** [generate ast] is a list of control flow graphs, where each control flow
    graph represents one top-level function, in addition to a list of foreign
    interface function names and a list of declared function (in-language)
    names. The returned value is a three-tuple as such. *)
val generate : AstType.prog -> Cfg.t list * string list * string list list
