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
| ':' { COLON }
| ',' { COMMA }
| "->" { ARROW }
| "Int" { INT_TYPE }
| "Bool" { BOOL_TYPE }
| "let" { LET }
| "print" { PRINT }
| "func" { FUNC }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "return" { RETURN }
| "true" { CONST_TRUE }
| "false" { CONST_FALSE }
| ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']* { IDEN (Lexing.lexeme lexbuf) }
| ['0'-'9']+ { INT_LIT (int_of_string (Lexing.lexeme lexbuf)) }
| _ as c
    { 
      let pos = Lexing.lexeme_start_p lexbuf in
      let lnum, cnum = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol + 1) in
      raise (LexerError (Printf.sprintf "Lexer error: unrecognized character '%c' at %s:%d:%d" c pos.pos_fname lnum cnum ))
    }
