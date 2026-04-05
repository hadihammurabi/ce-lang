%{
  open Ast
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING
%token <string> IDENT
%token          PLUS MINUS STAR SLASH
%token          LPAREN RPAREN
%token          COMMA
%token          NEWLINE
%token          EOF

%left  PLUS MINUS
%left  STAR SLASH
%nonassoc UMINUS

%start <Ast.stmt list> prog
%%

prog:
  | EOF                        { [] }
  | NEWLINE rest = prog        { rest }
  | s = stmt rest = prog       { s :: rest }

stmt:
  | e = expr NEWLINE           { Expr e }
  | e = expr EOF               { Expr e }

expr:
  | n = INT                                                        { Int n }
  | f = FLOAT                                                      { Float f }
  | s = STRING                                                     { String s }
  | id = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN   { Call (id, args) }
  | l = expr PLUS  r = expr                                        { Add (l, r) }
  | l = expr MINUS r = expr                                        { Sub (l, r) }
  | l = expr STAR  r = expr                                        { Mul (l, r) }
  | l = expr SLASH r = expr                                        { Div (l, r) }
  | MINUS e = expr %prec UMINUS                                    { Neg e }
  | LPAREN e = expr RPAREN                                         { e }
