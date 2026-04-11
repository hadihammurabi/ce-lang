%{
  open Ast
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING IDENT
%token <char>   CHAR
%token          PLUS MINUS STAR SLASH MOD EQEQ LT LTE GT GTE AND OR BANG
%token          LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA EQUALS DOT AMP SEMICOLON
%token          EOF RETURN IMPORT BREAK NEWLINE TYPE IMPL RAISE CATCH STRUCT INTERFACE
%token          TYPE_BOOL TRUE FALSE
%token          TYPE_VOID TYPE_STRING TYPE_CHAR TYPE_INT TYPE_FLOAT
%token          LET MUT FN IF ELSE FOR

%left OR AND
%left EQEQ LT LTE GT GTE
%left PLUS MINUS
%left STAR SLASH MOD
%nonassoc UMINUS

%start <Ast.stmt list> prog
%%

prog:
  | sep_opt EOF                 { [] }
  | sep_opt global_stmt_list EOF       { $2 }

sep:
  | NEWLINE       { () }
  | sep NEWLINE   { () }

sep_opt:
  |   { () }
  | sep           { () }

global_stmt_list:
  | global_stmt                           { [$1] }
  | global_stmt sep                       { [$1] }
  | global_stmt sep global_stmt_list      { $1 :: $3 }

global_stmt:
  | def_fn          { $1 }
  | def_let         { $1 }
  | def_type        { $1 }
  | def_struct      { $1 }
  | def_interface   { $1 }  /* <-- ADD THIS LINE */
  | IMPORT path = module_path { Import path }
  | IMPL struct_name = IDENT params = generic_params_opt LBRACE sep_opt methods = impl_method_list RBRACE
      { Impl (struct_name, params, methods) }

stmt:
  | def_fn          { $1 }
  | def_let         { $1 }
  | def_type        { $1 }
  | def_struct      { $1 }
  | def_interface   { $1 }  /* <-- ADD THIS LINE */
  | name = path EQUALS e = expr { Assign (name, e) }
  | name = path LBRACKET idx = expr RBRACKET EQUALS e = expr { ArrayAssign (name, idx, e) }
  | STAR name = path EQUALS e = expr { DerefAssign (Let name, e) }
  | RETURN expr     { Return $2 }
  | BREAK           { Break }
  | block           { Block $1 }
  | expr            { Expr $1 }
  | FOR body = block { For body }
  | IMPL struct_name = IDENT params = generic_params_opt LBRACE sep_opt methods = impl_method_list RBRACE
      { Impl (struct_name, params, methods) }
  | RAISE e = expr  { Raise e }  /* ADDED: raise expr */

block:
  | LBRACE sep_opt RBRACE             { [] }
  | LBRACE sep_opt stmt_list RBRACE   { $3 }

stmt_if:
  | IF e = expr body = block tail = stmt_if_tail
    { let (elif_branches, else_body) = tail in 
      ( If (e, body, elif_branches, else_body) ) }

stmt_if_tail:
  | { ([], None) }
  | ELSE IF e = expr body = block tail = stmt_if_tail 
      { let (elif_branches, else_body) = tail in ((e, body) :: elif_branches, else_body) }
  | ELSE body = block { ([], Some body) }

stmt_list:
  | stmt                  { [$1] }
  | stmt sep              { [$1] }
  | stmt sep stmt_list    { $1 :: $3 }

def_let:
  | LET name = IDENT ty = types EQUALS e = expr { DefLet (name, false, ty, Some e) }
  | LET MUT name = IDENT ty = types EQUALS e = expr { DefLet (name, true, ty, Some e) }
  | LET name = IDENT ty = types                 { DefLet (name, false, ty, None) }
  | LET MUT name = IDENT ty = types             { DefLet (name, true, ty, None) }

type_scalar:
  | TYPE_VOID  { TVoid }
  | TYPE_STRING{ TString }
  | TYPE_CHAR  { TChar }
  | TYPE_BOOL  { TBool }
  | TYPE_INT   { TInt }
  | TYPE_FLOAT { TFloat }
  | id = IDENT { TNamed id }

types:
  | t = type_scalar                       { t }
  | LBRACKET n = INT RBRACKET ty = types  { TArray (n, ty) }
  | STAR ty = types                       { TPointer ty }
  | name = IDENT LBRACKET arg_ty = types RBRACKET { TGenericInst (name, [arg_ty]) }
  | BANG ty = types                       { TResult ty }  /* ADDED: !T */

array:
  | LBRACKET n = INT RBRACKET t = type_scalar
    LBRACE elems = separated_list(COMMA, expr) RBRACE
    {  Array (n, t, elems) }

param:
  | name = IDENT ty = types { { param_name = name; ty = ty } }

def_fn:
  | FN name = IDENT LPAREN params = separated_list(COMMA, param) RPAREN ty = types body = block
    { DefFN (name, params, ty, body) }

def_type:
  | TYPE name = IDENT ty = types { DefType (name, ty) }

def_struct:
  | TYPE name = IDENT params = generic_params_opt STRUCT LBRACE sep_opt RBRACE 
    { DefStruct (name, params, []) }
  | TYPE name = IDENT params = generic_params_opt STRUCT LBRACE sep_opt fields = struct_field_list RBRACE 
    { DefStruct (name, params, fields) }

struct_field_list:
  | f = struct_field                                            { [f] }
  | f = struct_field SEMICOLON                                  { [f] }
  | f = struct_field sep                                        { [f] }
  | f = struct_field SEMICOLON sep_opt rest = struct_field_list { f :: rest }
  | f = struct_field sep rest = struct_field_list               { f :: rest }

struct_field:
  | name = IDENT ty = types { { field_name = name; ty = ty } }

struct_init_list:
  | f = struct_init_field                                            { [f] }
  | f = struct_init_field SEMICOLON                                  { [f] }
  | f = struct_init_field sep                                        { [f] }
  | f = struct_init_field SEMICOLON sep_opt rest = struct_init_list  { f :: rest }
  | f = struct_init_field sep rest = struct_init_list                { f :: rest }

struct_init_field:
  | name = IDENT EQUALS e = expr { (name, e) }

module_path:
  | IDENT { [$1] }
  | IDENT DOT module_path { $1 :: $3 }

path:
  | id = IDENT { id }
  | id = IDENT DOT p = path { id ^ "." ^ p }

generic_params_opt:
  | { [] }
  | LBRACKET name = IDENT ty = types RBRACKET { [(name, ty)] }

impl_method_list:
  |  { [] }
  | m = impl_method sep_opt rest = impl_method_list { m :: rest }

impl_method:
  | FN name = IDENT LPAREN self_id = IDENT RPAREN ret_ty = types LBRACE sep_opt body = stmt_list RBRACE
    { (name, self_id, [], ret_ty, body) }
  | FN name = IDENT LPAREN self_id = IDENT COMMA params = separated_list(COMMA, param) RPAREN ret_ty = types LBRACE sep_opt body = stmt_list RBRACE
    { (name, self_id, params, ret_ty, body) }

def_interface:
  | TYPE name = IDENT INTERFACE LBRACE sep_opt RBRACE 
      { DefInterface (name, []) }
  | TYPE name = IDENT INTERFACE LBRACE sep_opt sigs = fn_signature_list RBRACE 
      { DefInterface (name, sigs) }

fn_signature_list:
  | s = fn_signature                                            { [s] }
  | s = fn_signature SEMICOLON                                  { [s] }
  | s = fn_signature sep                                        { [s] }
  | s = fn_signature SEMICOLON sep_opt rest = fn_signature_list { s :: rest }
  | s = fn_signature sep rest = fn_signature_list               { s :: rest }

fn_signature:
  | FN name = IDENT LPAREN params = separated_list(COMMA, param) RPAREN ty = types 
      { { fn_name = name; params = params; ret_ty = ty } }

expr_simple:
  | a = array                                                     { a }
  | LPAREN e = expr RPAREN                                        { e }
  | TRUE                                                          { Bool true }
  | FALSE                                                         { Bool false }
  | n = INT                                                       { Int n }
  | f = FLOAT                                                     { Float f }
  | s = STRING                                                    { String s }
  | c = CHAR                                                      { Char c }
  | name = IDENT LBRACKET idx = expr RBRACKET                     { ArrayAccess (name, idx) }
  | id = path                                                     { Let id }
  | id = path LPAREN args = separated_list(COMMA, expr) RPAREN    { Call(id, args) }
  | MINUS e = expr %prec UMINUS                                   { Neg e }
  | AMP e = expr_simple                                           { Ref e }
  | STAR e = expr_simple                                          { Deref e }
  | name = IDENT DOT LBRACE sep_opt RBRACE                            { Struct (name, [], []) }
  | name = IDENT DOT LBRACE sep_opt fields = struct_init_list RBRACE  { Struct (name, [], fields) }
  | name = IDENT LBRACKET ty = types RBRACKET DOT LBRACE sep_opt RBRACE
    { Struct (name, [ty], []) }
  | name = IDENT LBRACKET ty = types RBRACKET DOT LBRACE sep_opt fields = struct_init_list RBRACE
    { Struct (name, [ty], fields) }
  | e = expr_simple CATCH LPAREN id = IDENT RPAREN ty = types body = block   
    { Catch(e, id, ty, body) }  /* Add this line */

expr:
  | e = expr_simple               { e }
  | l = expr PLUS  r = expr       {  Add (l, r) }
  | l = expr MINUS r = expr       {  Sub (l, r) }
  | l = expr STAR  r = expr       {  Mul (l, r) }
  | l = expr SLASH r = expr       {  Div (l, r) }
  | l = expr MOD   r = expr       {  Mod (l, r) }
  | l = expr EQEQ  r = expr       {  Eq (l, r) }
  | l = expr LT    r = expr       {  Lt (l, r) }
  | l = expr LTE   r = expr       {  Lte (l, r) }
  | l = expr GT    r = expr       {  Gt (l, r) }
  | l = expr GTE   r = expr       {  Gte (l, r) }
  | l = expr AND   r = expr       {  And (l, r) }
  | l = expr OR    r = expr       {  Or (l, r) }
  | stmt_if { $1 }
  
