open Ce_parser.Ast
open Opcode

type ctx = {
  mutable code : opcode list;
  mutable functions : (string * Ce_parser.Ast.expr list) list;
}

let make_ctx () = { code = []; functions = [] }

let emit ctx op = ctx.code <- op :: ctx.code

let finish ctx : opcode array = Array.of_list (List.rev ctx.code)

let register_function ctx name body =
  ctx.functions <- (name, body) :: ctx.functions

let rec compile_expr ctx = function
  | Int n    -> emit ctx (Push_int n)
  | Float f  -> emit ctx (Push_float f)
  | String s -> emit ctx (Push_string s)

  | Add (l, r) ->
    compile_expr ctx l;
    compile_expr ctx r;
    emit ctx Add

  | Sub (l, r) ->
    compile_expr ctx l;
    compile_expr ctx r;
    emit ctx Sub

  | Mul (l, r) ->
    compile_expr ctx l;
    compile_expr ctx r;
    emit ctx Mul

  | Div (l, r) ->
    compile_expr ctx l;
    compile_expr ctx r;
    emit ctx Div

  | Neg e ->
    compile_expr ctx e;
    emit ctx Neg

  | Call (fn, args) ->
    List.iter (compile_expr ctx) args;
    emit ctx (Call (fn, List.length args))

let compile_stmt ctx = function
  | Expr (Call (fn, args) as e) ->
    compile_expr ctx (Call (fn, args));
    ignore e

  | Expr e ->
    compile_expr ctx e;
    emit ctx Pop

  | DefFN (name, body) ->
    emit ctx (DefFN name);
    register_function ctx name body

let compile (stmts : stmt list) : program =
  let ctx = make_ctx () in
  List.iter (compile_stmt ctx) stmts;
  emit ctx Halt;
  { code = finish ctx; functions = ctx.functions }
