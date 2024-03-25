%{
  open Ast
%}

%token <int> CONST
%token <string> VAR
// %token PLUS MINUS TIMES PRINT ASSIGN LET
%token NEWLINE
%token DOLLAR

%start <Ast.expr list> main

%type <Ast.expr> expr
// %type <Ast.stmt> stmt

%%

main:
  | DOLLAR { [] }
  | expr NEWLINE main { $1 :: $3 }

expr:
  | CONST { Const $1 }
  | VAR { Var $1 }