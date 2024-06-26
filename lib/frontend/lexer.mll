{
  open Parser
  
  exception LexerError of string
}

rule read = parse
| eof { EOF }
| [' ' '\t' '\r'] { read lexbuf }
| '\n' { Lexing.new_line lexbuf; NEWLINE }
| "//" [^ '\n']* { NEWLINE } 
| "==" { EQUALS }
| '=' { ASSIGN }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
| '&' { BITAND }
| '(' { LPAR }
| ')' { RPAR }
| '{' { LBRACE }
| '}' { RBRACE }
| "::" { SCOPE }
| ':' { COLON }
| ',' { COMMA }
| "->" { ARROW }
| "Int" { INT_TYPE }
| "Bool" { BOOL_TYPE }
| "let" { LET }
| "print" { PRINT }
| "func" { FUNC }
| "@ffi" { FFI }
| "@decl" { DECL }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "return" { RETURN }
| "true" { CONST_TRUE }
| "false" { CONST_FALSE }
| "namespace" { NAMESPACE }
| ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']* { IDEN (Lexing.lexeme lexbuf) }
| ['0'-'9']+ { INT_LIT (int_of_string (Lexing.lexeme lexbuf)) }
| '"' { STRING_LIT (string_of_quoted (Buffer.create 16) lexbuf) }
| _ as c
    { 
      let pos = Lexing.lexeme_start_p lexbuf in
      let lnum, cnum = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1) in
      raise (LexerError (Printf.sprintf "Lexer error: unrecognized character '%c' at %s:%d:%d" c pos.pos_fname lnum cnum ))
    }

and string_of_quoted buf = parse
  | '\\' '"' { Buffer.add_char buf '\"'; string_of_quoted buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; string_of_quoted buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; string_of_quoted buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; string_of_quoted buf lexbuf }
  | '\\' 'r' { Buffer.add_char buf '\r'; string_of_quoted buf lexbuf }
  | '"' { Buffer.contents buf }
  | eof { raise End_of_file }
  | _ as c { Buffer.add_char buf c; string_of_quoted buf lexbuf }
