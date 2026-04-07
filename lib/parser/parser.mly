%{
  open Ast
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING IDENT
%token          PLUS MINUS STAR SLASH
%token          LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA EQUALS
%token          EOF
%token          FN VAR RETURN
%token          TYPE_INT TYPE_FLOAT TYPE_VOID

%left  PLUS MINUS
%left  STAR SLASH
%nonassoc UMINUS

%start <Ast.stmt list> prog
%%

prog:
  | EOF                 { [] }
  | stmt prog           { $1 :: $2}

stmt:
  | expr            { Expr $1 }
  | def_fn          { $1 }
  | def_var         { $1 }
  | RETURN e = expr  { Return e }
  | body = block    { Block body }

block:
  | LBRACE body = stmt_list RBRACE { body }

stmt_list:
  | { [] }
  | stmt stmt_list { $1 :: $2 }

def_var:
  | VAR name = IDENT ty = types EQUALS e = expr { DefVar (name, ty, e) }

type_scalar:
  | TYPE_INT   { TypeInt }
  | TYPE_FLOAT { TypeFloat }
  | TYPE_VOID  { TypeVoid }

types:
  | t = type_scalar                       { t }
  | LBRACKET n = INT RBRACKET ty = types  { TypeArray (n, ty) }

array:
  | LBRACKET n = INT RBRACKET t = type_scalar
    LBRACE elems = separated_list(COMMA, expr) RBRACE
    { Array (n, t, elems) }

param:
  | name = IDENT ty = types { Ast.{ name = name; ty = ty } }

def_fn:
  | FN name = IDENT LPAREN params = separated_list(COMMA, param) RPAREN ty = types body = block
    { DefFN (name, params, ty, body) }

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
  | id = IDENT                                                    { Var id }
  | a = array                                                     { a }
