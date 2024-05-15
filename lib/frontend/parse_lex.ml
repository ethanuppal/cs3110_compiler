exception ParserError of string

let function_auto_unit_return = function
  | AstType.Function { name; params; return; body } ->
      let last_stmt_is_return =
        if List.is_empty body then false
        else
          match List.rev body |> List.hd with
          | AstType.Return _ -> true
          | _ -> false
      in
      AstType.Function
        {
          name;
          params;
          return;
          body =
            (if return = Type.unit_prim_type && not last_stmt_is_return then
               body @ [ Return None ]
             else body);
        }
  | other -> other

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
      | AstType.Namespace { name; contents } ->
          AstType.Namespace
            { name; contents = List.map function_auto_unit_return contents }
      | AstType.Function func ->
          function_auto_unit_return (AstType.Function func)
      | other -> other)
    prog
