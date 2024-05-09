exception ParserError of string

(** [lex_and_parse input] is the list of statements represented by the source
    code string [input].

    @raise ParserError on parsing error. *)
let lex_and_parse input =
  let syntax_error_msg lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    let lnum, cnum = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol) in
    Printf.sprintf "Syntax error at line %d, character %d" lnum cnum
  in
  let parse lexbuf = Parser.main Lexer.read lexbuf in
  let lexbuf = Lexing.from_string input in
  try parse lexbuf
  with Parser.Error -> raise (ParserError (syntax_error_msg lexbuf))
