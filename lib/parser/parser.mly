%{
  open Ast
%}

%token <int>    INT
%token <float>  FLOAT
%token <string> STRING IDENT
%token <char>   CHAR
%token          PLUS MINUS STAR SLASH MOD EQEQ LT LTE GT GTE AND OR BANG
%token          LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA EQUALS DOT AMP SEMICOLON
%token          EOF RETURN IMPORT BREAK NEWLINE TYPE IMPL RAISE CATCH STRUCT TRAIT
%token          TYPE_BOOL TYPE_VOID TYPE_STRING TYPE_CHAR
%token          TYPE_INT TYPE_I8 TYPE_I16 TYPE_I32 TYPE_I64 TYPE_I128
%token          TYPE_UINT TYPE_U8 TYPE_U16 TYPE_U32 TYPE_U64 TYPE_U128
%token          TYPE_FLOAT TYPE_F32 TYPE_F64
%token          LET MUT TRUE FALSE FN IF ELSE FOR NIL

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
  | def_trait   { $1 }
  | IMPORT path = module_path { Import path }
  | IMPL struct_name = impl_target params = generic_params_opt LBRACE sep_opt methods = impl_method_list RBRACE
      { Impl (struct_name, params, methods) }

stmt:
  | def_fn          { $1 }
  | def_let         { $1 }
  | def_type        { $1 }
  | def_struct      { $1 }
  | def_trait   { $1 }
  | name = path EQUALS e = expr { Assign (name, e) }
  | name = path LBRACKET idx = expr RBRACKET EQUALS e = expr { ArrayAssign (name, idx, e) }
  | STAR ptr = expr_simple EQUALS e = expr { DerefAssign (ptr, e) }
  | RETURN expr     { Return $2 }
  | BREAK           { Break }
  | block           { Block $1 }
  | expr            { Expr $1 }
  | RAISE e = expr  { Raise e }
  | IMPL struct_name = impl_target params = generic_params_opt LBRACE sep_opt methods = impl_method_list RBRACE
      { Impl (struct_name, params, methods) }
  | FOR body = block { For (None, None, None, body) }
  | FOR cond = expr_no_struct body = block { For (None, Some cond, None, body) }
  | FOR init = for_init SEMICOLON cond = expr_no_struct body = block { For (Some init, Some cond, None, body) }
  | FOR init = for_init SEMICOLON cond = expr_no_struct SEMICOLON mut = for_mut body = block { For (Some init, Some cond, Some mut, body) }

block:
  | LBRACE sep_opt RBRACE             { [] }
  | LBRACE sep_opt stmt_list RBRACE   { $3 }

stmt_if:
  | IF e = expr_no_struct body = block tail = stmt_if_tail
    { let (elif_branches, else_body) = tail in 
      ( If (e, body, elif_branches, else_body) ) }

stmt_if_tail:
  | { ([], None) }
  | ELSE IF e = expr_no_struct body = block tail = stmt_if_tail 
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

  | LET name = IDENT EQUALS e = expr            { DefLet (name, false, TUnknown, Some e) }
  | LET MUT name = IDENT EQUALS e = expr        { DefLet (name, true, TUnknown, Some e) }

type_scalar:
  | TYPE_VOID  { TVoid }
  | TYPE_STRING{ TString }
  | TYPE_CHAR  { TChar }
  | TYPE_BOOL  { TBool }
  | TYPE_INT   { TI32 } | TYPE_I8 { TI8 } | TYPE_I16 { TI16 } | TYPE_I32 { TI32 } | TYPE_I64 { TI64 } | TYPE_I128 { TI128 }
  | TYPE_UINT  { TU32 } | TYPE_U8 { TU8 } | TYPE_U16 { TU16 } | TYPE_U32 { TU32 } | TYPE_U64 { TU64 } | TYPE_U128 { TU128 }
  | TYPE_FLOAT { TF64 } | TYPE_F32 { TF32 } | TYPE_F64 { TF64 }
  | id = path { TNamed id }

types:
  | t = type_scalar                       { t }
  | LBRACKET n = INT RBRACKET ty = types  { TArray (n, ty) }
  | STAR ty = types                       { TPointer ty }
  | name = path LT arg_ty = types GT { TGenericInst (name, [arg_ty]) }
  | BANG ty = types                       { TResult ty }
  | LPAREN t = types COMMA rest = separated_nonempty_list(COMMA, types) RPAREN { TTuple (t :: rest) }
  | FN LPAREN RPAREN ret_ty = types { TFn([], ret_ty) }
  | FN LPAREN arg_tys = separated_nonempty_list(COMMA, types) RPAREN ret_ty = types { TFn(arg_tys, ret_ty) }

array:
  | LBRACKET n = INT RBRACKET t = type_scalar
    LBRACE elems = separated_list(COMMA, expr) RBRACE
    {  Array (n, t, elems) }

param:
  | name = IDENT ty = types { { param_name = name; ty = ty } }

def_fn:
  | FN name = IDENT tparams = generic_params_opt LPAREN params = separated_list(COMMA, param) RPAREN ty = types body = block
    { DefFN (name, tparams, params, ty, body) }

def_type:
  | TYPE name = IDENT ty = types { DefType (name, ty) }

def_struct:
  | STRUCT name = IDENT params = generic_params_opt LBRACE sep_opt RBRACE 
    { DefStruct (name, params, []) }
  | STRUCT name = IDENT params = generic_params_opt LBRACE sep_opt fields = struct_field_list RBRACE 
    { DefStruct (name, params, fields) }

struct_field_list:
  | f = struct_field                                            { [f] }
  | f = struct_field SEMICOLON                                  { [f] }
  | f = struct_field sep                                        { [f] }
  | f = struct_field SEMICOLON sep_opt rest = struct_field_list { f :: rest }
  | f = struct_field sep rest = struct_field_list               { f :: rest }

struct_field:
  | name = IDENT ty = types             { { field_name = name; ty = ty; is_mut = false } }
  | MUT name = IDENT ty = types         { { field_name = name; ty = ty; is_mut = true } }

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
  | LT name = IDENT ty = types GT { [(name, ty)] }

impl_method_list:
  |  { [] }
  | m = impl_method sep_opt rest = impl_method_list { m :: rest }

impl_method:
  | FN name = IDENT LPAREN self_id = IDENT RPAREN ret_ty = types LBRACE sep_opt body = stmt_list RBRACE
    { (name, self_id, false, [], ret_ty, body) }
  | FN name = IDENT LPAREN AMP self_id = IDENT RPAREN ret_ty = types LBRACE sep_opt body = stmt_list RBRACE
    { (name, self_id, true, [], ret_ty, body) }
  | FN name = IDENT LPAREN self_id = IDENT COMMA params = separated_list(COMMA, param) RPAREN ret_ty = types LBRACE sep_opt body = stmt_list RBRACE
    { (name, self_id, false, params, ret_ty, body) }
  | FN name = IDENT LPAREN AMP self_id = IDENT COMMA params = separated_list(COMMA, param) RPAREN ret_ty = types LBRACE sep_opt body = stmt_list RBRACE
    { (name, self_id, true, params, ret_ty, body) }

impl_target:
  | id = IDENT { id }
  | TYPE_INT | TYPE_I32 { "int" }
  | TYPE_FLOAT | TYPE_F64 { "float" }
  | TYPE_UINT | TYPE_U32 { "uint" }
  | TYPE_STRING { "string" }
  | TYPE_BOOL { "bool" }
  | TYPE_CHAR { "char" }
  | TYPE_I8 { "i8" } | TYPE_I16 { "i16" } | TYPE_I64 { "i64" } | TYPE_I128 { "i128" }
  | TYPE_U8 { "u8" } | TYPE_U16 { "u16" } | TYPE_U64 { "u64" } | TYPE_U128 { "u128" }
  | TYPE_F32 { "f32" }

def_trait:
  | TRAIT name = IDENT LBRACE sep_opt RBRACE 
      { DefInterface (name, []) }
  | TRAIT name = IDENT LBRACE sep_opt sigs = fn_signature_list RBRACE 
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

for_init:
  | name = IDENT ty = types EQUALS e = expr { DefLet (name, true, ty, Some e) }
  | name = IDENT EQUALS e = expr            { DefLet (name, true, TUnknown, Some e) }

for_mut:
  | name = path EQUALS e = expr_no_struct { Assign (name, e) }
  | name = path LBRACKET idx = expr RBRACKET EQUALS e = expr_no_struct { ArrayAssign (name, idx, e) }
  | STAR ptr = expr_simple EQUALS e = expr_no_struct { DerefAssign (ptr, e) }
  | e = expr_no_struct { Expr e }

expr_simple:
  | a = array                                                     { a }
  | LPAREN e = expr RPAREN                                        { e }
  | NIL                                                           { Nil }        
  | TRUE                                                          { Bool true }
  | FALSE                                                         { Bool false }
  | n = INT                                                       { Int n }
  | f = FLOAT                                                     { Float f }
  | s = STRING                                                    { String s }
  | c = CHAR                                                      { Char c }
  | id = path                                                     { Let id }
  | MINUS e = expr %prec UMINUS                                   { Neg e }
  | BANG e = expr %prec UMINUS                                    { Not e }
  | AMP e = expr_simple                                           { Ref e }
  | STAR e = expr_simple                                          { Deref e }
  | name = path LBRACKET idx = expr RBRACKET                      { ArrayAccess (name, idx) }
  
  | name = path LBRACE sep_opt RBRACE                             { Struct (name, [], []) }
  | name = path LBRACE sep_opt fields = struct_init_list RBRACE   { Struct (name, [], fields) }
  | id = path LPAREN args = separated_list(COMMA, expr) RPAREN    { Call(id, [], args) }

  | e = expr LT targs = separated_list(COMMA, types) GT LPAREN args = separated_list(COMMA, expr) RPAREN
    { match e with Let id -> Call(id, targs, args) | _ -> raise (Failure "Invalid generic function call") }
  | e = expr LT targs = separated_list(COMMA, types) GT LBRACE sep_opt RBRACE
    { match e with Let name -> Struct (name, targs, []) | _ -> raise (Failure "Invalid generic struct instantiation") }
  | e = expr LT targs = separated_list(COMMA, types) GT LBRACE sep_opt fields = struct_init_list RBRACE
    { match e with Let name -> Struct (name, targs, fields) | _ -> raise (Failure "Invalid generic struct instantiation") }

  | e = expr_simple CATCH LPAREN id = IDENT RPAREN ty = types body = block { Catch(e, id, ty, body) }
  | e = expr_simple CATCH handler = expr_simple { CatchExpr(e, handler) }
  | LPAREN e = expr COMMA rest = separated_nonempty_list(COMMA, expr) RPAREN { Tuple (e :: rest) }
  | FN LPAREN RPAREN ty = types body = block { AnonFN([], ty, body) }
  | FN LPAREN params = separated_nonempty_list(COMMA, param) RPAREN ty = types body = block { AnonFN(params, ty, body) }

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
  
expr_simple_no_struct:
  | a = array                                                     { a }
  | NIL                                                           { Nil }
  | LPAREN e = expr RPAREN                                        { e }
  | TRUE                                                          { Bool true }
  | FALSE                                                         { Bool false }
  | n = INT                                                       { Int n }
  | f = FLOAT                                                     { Float f }
  | s = STRING                                                    { String s }
  | c = CHAR                                                      { Char c }
  | id = path                                                     { Let id }
  | MINUS e = expr_no_struct %prec UMINUS                         { Neg e }
  | BANG e = expr_no_struct %prec UMINUS                          { Not e }
  | AMP e = expr_simple_no_struct                                 { Ref e }
  | STAR e = expr_simple_no_struct                                { Deref e }
  | name = path LBRACKET idx = expr RBRACKET                      { ArrayAccess (name, idx) }
  | id = path LPAREN args = separated_list(COMMA, expr) RPAREN    { Call(id, [], args) }
  | e = expr_no_struct LT targs = separated_list(COMMA, types) GT LPAREN args = separated_list(COMMA, expr) RPAREN
    { match e with Let id -> Call(id, targs, args) | _ -> raise (Failure "Invalid generic function call") }
  | LPAREN e = expr_no_struct COMMA rest = separated_nonempty_list(COMMA, expr) RPAREN { Tuple (e :: rest) }
  | FN LPAREN RPAREN ty = types body = block { AnonFN([], ty, body) }
  | FN LPAREN params = separated_nonempty_list(COMMA, param) RPAREN ty = types body = block { AnonFN(params, ty, body) }

expr_no_struct:
  | e = expr_simple_no_struct                     { e }
  | l = expr_no_struct PLUS  r = expr_no_struct   { Add (l, r) }
  | l = expr_no_struct MINUS r = expr_no_struct   { Sub (l, r) }
  | l = expr_no_struct STAR  r = expr_no_struct   { Mul (l, r) }
  | l = expr_no_struct SLASH r = expr_no_struct   { Div (l, r) }
  | l = expr_no_struct MOD   r = expr_no_struct   { Mod (l, r) }
  | l = expr_no_struct EQEQ  r = expr_no_struct   { Eq (l, r) }
  | l = expr_no_struct LT    r = expr_no_struct   { Lt (l, r) }
  | l = expr_no_struct LTE   r = expr_no_struct   { Lte (l, r) }
  | l = expr_no_struct GT    r = expr_no_struct   { Gt (l, r) }
  | l = expr_no_struct GTE   r = expr_no_struct   { Gte (l, r) }
  | l = expr_no_struct AND   r = expr_no_struct   { And (l, r) }
  | l = expr_no_struct OR    r = expr_no_struct   { Or (l, r) }
  | stmt_if { $1 }

