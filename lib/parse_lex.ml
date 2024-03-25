let lex_and_parse input = 
  let lexbuf = Lexing.from_string input in
  try
    let parsed = Parser.main Lexer.read lexbuf in
    List.map Ast.string_of_expr parsed |> String.concat " " |> print_endline
  with
  | Parser.Error -> failwith "Parser Error"