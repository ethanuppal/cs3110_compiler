exception ParserError of string

(** [lex_and_parse ~filename:filename input] is the list of statements
    represented by the source code string [input]. Optionally,
    [~filename:filename] can be passed to indicate that the path of the source
    was [filename]; by default, it is ["<stdin>"].

    @raise ParserError on parsing error. *)
let lex_and_parse ?(filename = "<stdin>") input =
  let syntax_error_msg lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    let lnum, cnum = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1) in
    Printf.sprintf "Syntax error at %s:%d:%d" pos.pos_fname lnum cnum
  in
  let parse lexbuf = Parser.main Lexer.read lexbuf in
  let lexbuf = Lexing.from_string ~with_positions:true input in
  Lexing.set_filename lexbuf filename;
  let prog =
    try parse lexbuf with
    | Parser.Error -> raise (ParserError (syntax_error_msg lexbuf))
    | Lexer.LexerError err -> raise (ParserError err)
  in
  List.map
    (fun stmt ->
      match stmt with
      | Ast.Function { name; params; return; body } ->
          let last_stmt_is_return =
            if List.is_empty body then false
            else
              match List.rev body |> List.hd with
              | Ast.Return _ -> true
              | _ -> false
          in
          Ast.Function
            {
              name;
              params;
              return;
              body =
                (if return = Type.unit_prim_type && not last_stmt_is_return then
                   body @ [ Return None ]
                 else body);
            }
      | other -> other)
    prog
