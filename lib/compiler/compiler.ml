open Ce_parser.Ast
open Opcode

type ctx = {
  mutable code : opcode list;
  mutable functions : (string * Ce_parser.Ast.types * Ce_parser.Ast.stmt list) list;
  mutable globals : (string * Ce_parser.Ast.types * Ce_parser.Ast.expr) list;
}

let make_ctx () = { code = []; functions = []; globals = [] }

let emit ctx op = ctx.code <- op :: ctx.code

let finish ctx : opcode array = Array.of_list (List.rev ctx.code)

let register_function ctx name ty body =
  ctx.functions <- (name, ty, body) :: ctx.functions

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

  | Var name -> emit ctx (Var name)

let compile_stmt ctx = function
  | Expr (Call (fn, args) as e) ->
    compile_expr ctx (Call (fn, args));
    ignore e

  | Expr e ->
    compile_expr ctx e;
    emit ctx Pop

  | DefFN (name, ty, body) ->
    emit ctx (DefFN name);
    register_function ctx name ty body

  | DefVar (name, ty, value) ->
    emit ctx (DefVar name);
    ctx.globals <- (name, ty, value) :: ctx.globals

  | Return e ->
    compile_expr ctx e;
    emit ctx Return

let compile (stmts : stmt list) : program =
  let ctx = make_ctx () in
  List.iter (compile_stmt ctx) stmts;
  emit ctx Halt;
  { code = finish ctx; functions = ctx.functions; globals = List.rev ctx.globals }

let compile_expr_to_program (expr : Ce_parser.Ast.expr) : Opcode.program =
  let ctx = make_ctx () in
  compile_expr ctx expr;
  emit ctx Pop;
  emit ctx Halt;
  { code = finish ctx; functions = ctx.functions; globals = List.rev ctx.globals }

let export output_file (prog: program)=
  let base_name = try String.sub output_file 0 (String.rindex output_file '.') with Not_found -> output_file in
  let c_file = base_name ^ ".c" in
  Bytecode.write_c_wrapper c_file prog.code prog.functions prog.globals;
  Linker.compile_and_link output_file c_file;
  Linker.cleanup_temp c_file
