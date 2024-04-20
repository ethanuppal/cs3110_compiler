(** [generate ast] is a list of control flow graphs, where each control flow
    graph represents one top-level function.

    TEMPORARY: only one function is allowed at the top level. It must be called
    main. Only local variables, print statements, mathematical expressions, and
    if statements are supported. *)
val generate : Ast.prog -> Cfg.t list
