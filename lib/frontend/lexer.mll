{
  open Parser
  
  exception LexerError of string
}

rule read = parse
| eof { EOF }
| [' ' '\t' '\r'] { read lexbuf }
| '\n' { NEWLINE }
| "//" [^ '\n']* '\n' { NEWLINE }
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
| "true" { CONST_TRUE }
| "false" { CONST_FALSE }
| ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']* { IDEN (Lexing.lexeme lexbuf) }
| ['0'-'9']+ { INT_LIT (int_of_string (Lexing.lexeme lexbuf)) }
| _ as c
    { 
      let pos = Lexing.lexeme_start_p lexbuf in
      let lnum, cnum = pos.pos_lnum, (pos.pos_cnum - pos.pos_bol) in
      let l1 = Printf.sprintf "Error at line %d, character %d. " lnum cnum in
      let l2 = Printf.sprintf "Unrecognized character: [%c]" c in
      raise (LexerError (l1 ^ l2))
    }
