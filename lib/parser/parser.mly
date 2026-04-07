%{
  open Ast
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING IDENT
%token          PLUS MINUS STAR SLASH
%token          LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA EQUALS
%token          NEWLINE EOF
%token          FN VAR RETURN
%token          TYPE_INT TYPE_FLOAT TYPE_VOID

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
  | def_var terminator        { $1 }
  | RETURN e = expr terminator { Return e }

def_var:
  | VAR name = IDENT ty = types EQUALS e = expr { DefVar (name, ty, e) }
  | VAR name = IDENT ty = types EQUALS a = array { DefVar (name, ty, a) }

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
  | FN name = IDENT LPAREN params = separated_list(COMMA, param) RPAREN ty = types
    LBRACE newline body = def_fn_body newline RBRACE
    { DefFN (name, params, ty, body) }

def_fn_body:
  | { [] }
  | stmt def_fn_body { $1 :: $2 }

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
  | id = IDENT                                                    { Var id }
  | a = array                                                 { a }
