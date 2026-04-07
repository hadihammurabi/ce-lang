%{
  open Ast
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING IDENT
%token          PLUS MINUS STAR SLASH MOD EQEQ LT LTE GT GTE AND OR
%token          LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA EQUALS DOT
%token          EOF RETURN IMPORT BREAK
%token          TYPE_BOOL TRUE FALSE
%token          TYPE_VOID TYPE_STRING TYPE_INT TYPE_FLOAT
%token          LET MUT FN IF ELSE FOR

%left OR AND
%left EQEQ LT LTE GT GTE
%left PLUS MINUS
%left STAR SLASH MOD
%nonassoc UMINUS

%start <Ast.stmt list> prog
%%

prog:
  | EOF                 { [] }
  | stmt prog           { $1 :: $2}

stmt:
  | def_fn          { $1 }
  | def_let         { $1 }
  | name = path EQUALS e = expr { Assign (name, e) }
  | RETURN expr     { Return $2 }
  | BREAK           { Break }
  | block           { Block $1 }
  | expr            { Expr $1 }
  | FOR body = block { For body }
  | IMPORT path = module_path { Import path }

block:
  | LBRACE body = stmt_list RBRACE { body }

stmt_if:
  | IF e = expr body = block tail = stmt_if_tail
    { let (elif_branches, else_body) = tail in 
      If (e, body, elif_branches, else_body) }

stmt_if_tail:
  | { ([], None) }
  | ELSE IF e = expr body = block tail = stmt_if_tail 
      { let (elif_branches, else_body) = tail in ((e, body) :: elif_branches, else_body) }
  | ELSE body = block { ([], Some body) }

stmt_list:
  | { [] }
  | stmt stmt_list { $1 :: $2 }

def_let:
  | LET name = IDENT ty = types EQUALS e = expr { DefLet (name, false, ty, e) }
  | LET MUT name = IDENT ty = types EQUALS e = expr { DefLet (name, true, ty, e) }

type_scalar:
  | TYPE_VOID  { TypeVoid }
  | TYPE_STRING{ TypeString }
  | TYPE_BOOL  { TypeBool }
  | TYPE_INT   { TypeInt }
  | TYPE_FLOAT { TypeFloat }

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

module_path:
  | IDENT { [$1] }
  | IDENT DOT module_path { $1 :: $3 }

path:
  | id = IDENT { id }
  | id = IDENT DOT p = path { id ^ "." ^ p }

expr_simple:
  | TRUE                          { Bool true }
  | FALSE                         { Bool false }
  | n = INT                                                       { Int n }
  | f = FLOAT                                                     { Float f }
  | s = STRING                                                    { String s }
  | id = path                                                    { Let id }
  | id = path LPAREN args = separated_list(COMMA, expr) RPAREN   { Call (id, args) }
  | MINUS e = expr %prec UMINUS                                   { Neg e }
  | LPAREN e = expr RPAREN                                        { e }
  | a = array                                                     { a }

expr:
  | e = expr_simple               { e }
  | l = expr PLUS  r = expr       { Add (l, r) }
  | l = expr MINUS r = expr       { Sub (l, r) }
  | l = expr STAR  r = expr       { Mul (l, r) }
  | l = expr SLASH r = expr       { Div (l, r) }
  | l = expr MOD   r = expr       { Mod (l, r) }
  | l = expr EQEQ  r = expr       { Eq (l, r) }
  | l = expr LT    r = expr       { Lt (l, r) }
  | l = expr LTE   r = expr       { Lte (l, r) }
  | l = expr GT    r = expr       { Gt (l, r) }
  | l = expr GTE   r = expr       { Gte (l, r) }
  | l = expr AND   r = expr       { And (l, r) }
  | l = expr OR    r = expr       { Or (l, r) }
  | stmt_if { $1 }
  
