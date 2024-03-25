%{
  open Ast
%}

%token <int> CONST
%token <string> VAR
%token PLUS MINUS TIMES DIVIDE MOD 
%token LPAR RPAR LBRACE RBRACE
%token PRINT ASSIGN LET FUNC
%token NEWLINE
%token EOF

%start <Ast.stmt list> main

%type <Ast.stmt> stmt
%type <Ast.expr> expr

%%

main:
  | EOF { [] }
  | NEWLINE main { $2 }
  | stmt NEWLINE main { $1 :: $3 }

expr:
  | CONST { Const $1 }
  | VAR { Var $1 }
  | expr PLUS expr { Infix {lhs = $1; op = Plus; rhs = $3} }
  | expr MINUS expr { Infix {lhs = $1; op = Minus; rhs = $3} }
  | expr TIMES expr { Infix {lhs = $1; op = Times; rhs = $3} }
  | expr DIVIDE expr { Infix {lhs = $1; op = Divide; rhs = $3} }
  | expr MOD expr { Infix {lhs = $1; op = Mod; rhs = $3} }
  | PLUS expr { Prefix {op = Plus; rhs = $2} }
  | MINUS expr { Prefix {op = Minus; rhs = $2} }
  | TIMES expr { Prefix {op = Times; rhs = $2} }

body_till_rbrace:
  | NEWLINE body_till_rbrace { $2 } 
  | RBRACE {[]}
  | stmt RBRACE {[$1]}
  | stmt NEWLINE body_till_rbrace { $1 :: $3 }

stmt:
  | VAR LPAR RPAR { Call $1 }
  | LET VAR ASSIGN expr { Declaration ($2, $4) }
  | VAR ASSIGN expr { Assignment ($1, $3) }
  | FUNC VAR LPAR RPAR LBRACE body_till_rbrace { Function {name = $2; body = $6} }
  | PRINT expr { Print $2 }
