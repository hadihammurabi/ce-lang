%{
  open Ast
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING IDENT
%token          PLUS MINUS STAR SLASH
%token          LPAREN RPAREN LBRACE RBRACE COMMA
%token          NEWLINE EOF
%token          FN

%left  PLUS MINUS
%left  STAR SLASH
%nonassoc UMINUS

%start <Ast.stmt list> prog
%%

prog:
  | EOF                 { [] }
  | NEWLINE prog        { $2 }
  | stmt prog           { $1 :: $2}

stmt:
  | expr terminator           { Expr $1 }
  | def_fn terminator         { $1 }

def_fn:
    | FN name = IDENT LPAREN RPAREN LBRACE newline body = def_fn_body newline RBRACE { DefFN (name, body) }

def_fn_body:
  | { [] }
  | expr terminator def_fn_body { $1 :: $3 }

newline:
  | { }
  | NEWLINE newline { }

terminator:
  | NEWLINE | EOF        {}

expr:
  | n = INT                                                       { Int n }
  | f = FLOAT                                                     { Float f }
  | s = STRING                                                    { String s }
  | l = expr PLUS  r = expr                                       { Add (l, r) }
  | l = expr MINUS r = expr                                       { Sub (l, r) }
  | l = expr STAR  r = expr                                       { Mul (l, r) }
  | l = expr SLASH r = expr                                       { Div (l, r) }
  | MINUS e = expr %prec UMINUS                                   { Neg e }
  | LPAREN e = expr RPAREN                                        { e }
  | id = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN   { Call (id, args) }
