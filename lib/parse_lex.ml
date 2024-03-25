let lex_and_parse input = 
  let lexbuf = Lexing.from_string input in
  try
    let parsed = Parser.main Lexer.read lexbuf in
    Ast.string_of_prog parsed |> print_endline
  with
  | Parser.Error -> failwith "Parser Error"