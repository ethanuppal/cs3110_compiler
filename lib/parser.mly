%{
  open Ast
%}

%token <int> CONST
%token <string> VAR
%token PLUS MINUS TIMES PRINT ASSIGN LET
%token EOF
%token LINEBREAK

%start <Ast.main> main
%type <Ast.expr> expr

%%

main:
  | expr EOF { $1 }

expr:
  | CONST { Const $1 }
  | VAR { Var $1 }
  | LET VAR ASSIGN expr { Declaration ($2, $4) }
  | expr PLUS expr { Infix {lhs = $1; op = Plus; rhs = $3} }
  | expr MINUS expr { Infix {lhs = $1; op = Minus; rhs = $3} }
  | expr TIMES expr { Infix {lhs = $1; op = Times; rhs = $3} }
  | PRINT expr { Unary {op = Print; rhs = $2} }