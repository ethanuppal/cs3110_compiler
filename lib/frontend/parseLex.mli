(** An error indicating that parsing failed with an attached message. *)
exception ParserError of string

(** [lex_and_parse ~filename:filename input] is the list of statements
    represented by the source code string [input]. Optionally,
    [~filename:filename] can be passed to indicate that the path of the source
    was [filename]; by default, it is ["<stdin>"].

    @raise ParserError on parsing error. *)
val lex_and_parse : ?filename:string -> string -> AstType.stmt list
