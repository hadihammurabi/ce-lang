open Ce_parser
open Opcode

type ctx = {
  mutable code : opcode list;
  mutable functions :
    (string * Ast.param list * Ast.types * Ast.stmt list) list;
  mutable globals : (string * Ast.types * Ast.expr) list;
}

let make_ctx () = { code = []; functions = []; globals = [] }
let emit ctx op = ctx.code <- op :: ctx.code
let finish ctx : opcode array = Array.of_list (List.rev ctx.code)

let register_function ctx name params ty body =
  ctx.functions <- (name, params, ty, body) :: ctx.functions

let rec compile_expr ctx = function
  | Ast.Int n -> emit ctx (Push_int n)
  | Ast.Float f -> emit ctx (Push_float f)
  | Ast.String s -> emit ctx (Push_string s)
  | Ast.Add (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Add
  | Ast.Sub (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Sub
  | Ast.Mul (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Mul
  | Ast.Div (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Div
  | Ast.Eq (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Eq
  | Ast.Lt (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Lt
  | Ast.Gt (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Gt
  | Ast.Lte (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Lte
  | Ast.Gte (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Gte
  | Ast.And (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx And
  | Ast.Or (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Or
  | Ast.Bool b -> emit ctx (Push_bool b)
  | Ast.Neg e ->
      compile_expr ctx e;
      emit ctx Neg
  | Ast.Call (fn, args) ->
      List.iter (compile_expr ctx) args;
      emit ctx (Call (fn, List.length args))
  | Ast.Var name -> emit ctx (Var name)
  | Ast.Array (n, ty, elems) ->
      List.iter (compile_expr ctx) elems;
      emit ctx (Push_array (n, ty))
  | Ast.If (cond, then_body, elif_branches, else_body) -> (
      compile_expr ctx cond;
      List.iter (compile_stmt ctx) then_body;
      List.iter
        (fun (econd, ebody) ->
          compile_expr ctx econd;
          List.iter (compile_stmt ctx) ebody)
        elif_branches;
      match else_body with
      | Some stmts -> List.iter (compile_stmt ctx) stmts
      | None -> ())

and compile_stmt ctx = function
  | Ast.Expr (Call (fn, args) as e) ->
      compile_expr ctx (Call (fn, args));
      ignore e
  | Ast.Expr e ->
      compile_expr ctx e;
      emit ctx Pop
  | Ast.DefFN (name, params, ty, body) ->
      emit ctx (DefFN (name, params));
      register_function ctx name params ty body
  | Ast.DefVar (name, ty, value) ->
      emit ctx (DefVar name);
      ctx.globals <- (name, ty, value) :: ctx.globals
  | Ast.Return e ->
      compile_expr ctx e;
      emit ctx Return
  | Ast.Block body -> List.iter (compile_stmt ctx) body
  | Ast.For body -> List.iter (compile_stmt ctx) body
  | Ast.Break -> emit ctx Break

let compile (stmts : Ast.stmt list) : program =
  let ctx = make_ctx () in
  List.iter (compile_stmt ctx) stmts;
  emit ctx Halt;
  {
    code = finish ctx;
    functions = ctx.functions;
    globals = List.rev ctx.globals;
  }

let compile_expr_to_program (expr : Ast.expr) : Opcode.program =
  let ctx = make_ctx () in
  compile_expr ctx expr;
  emit ctx Pop;
  emit ctx Halt;
  {
    code = finish ctx;
    functions = ctx.functions;
    globals = List.rev ctx.globals;
  }

let export output_file (prog : program) =
  let base_name =
    try String.sub output_file 0 (String.rindex output_file '.')
    with Not_found -> output_file
  in
  let c_file = base_name ^ ".c" in
  Bytecode.write_c_wrapper c_file prog.code prog.functions prog.globals;
  Linker.compile_and_link output_file c_file;
  Linker.cleanup_temp base_name
