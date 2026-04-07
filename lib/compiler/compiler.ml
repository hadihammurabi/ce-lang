open Ce_parser
open Opcode

type ctx = {
  mutable code : opcode list;
  mutable functions :
    (string * Ast.param list * Ast.types * Ast.stmt list) list;
  mutable globals : (string * bool * Ast.types * Ast.expr) list;
  env : (string, bool) Hashtbl.t;
}

let make_ctx () =
  { code = []; functions = []; globals = []; env = Hashtbl.create 16 }

let emit ctx op = ctx.code <- op :: ctx.code
let finish ctx : opcode array = Array.of_list (List.rev ctx.code)

let register_function ctx name params ty body =
  ctx.functions <- (name, params, ty, body) :: ctx.functions

let rec compile_expr ctx (e : Ast.expr) = match e.kind with
  | Ast.Void -> ()
  | Ast.Int n -> emit ctx (Push_int n)
  | Ast.Float f -> emit ctx (Push_float f)
  | Ast.String s -> emit ctx (Push_string s)
  | Ast.Bool b -> emit ctx (Push_bool b)
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
  | Ast.Mod (l, r) ->
      compile_expr ctx l;
      compile_expr ctx r;
      emit ctx Mod
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
  | Ast.Neg e ->
      compile_expr ctx e;
      emit ctx Neg
  | Ast.Call (fn, args) ->
      List.iter (compile_expr ctx) args;
      emit ctx (Call (fn, List.length args))
  | Ast.Let name -> emit ctx (Let name)
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
  | Ast.Expr ({ kind = Ast.Call (fn, args); _ } as e) ->
      compile_expr ctx e;
      ignore e
  | Ast.Expr e ->
      compile_expr ctx e;
      emit ctx Pop
  | Ast.DefFN (name, params, ty, body) ->
      emit ctx (DefFN (name, params));
      register_function ctx name params ty body;

      let previous_env = Hashtbl.copy ctx.env in
      List.iter (fun p -> Hashtbl.add ctx.env p.Ast.name false) params;
      List.iter (compile_stmt ctx) body;
      Hashtbl.clear ctx.env;
      Hashtbl.iter (fun k v -> Hashtbl.add ctx.env k v) previous_env
  | Ast.DefLet (name, ismut, ty, value) ->
      compile_expr ctx value;
      Hashtbl.add ctx.env name ismut;
      emit ctx (DefLet (name, ismut));
      ctx.globals <- (name, ismut, ty, value) :: ctx.globals
  | Ast.Assign (name, expr) ->
      (match Hashtbl.find_opt ctx.env name with
      | None -> failwith (Printf.sprintf "Variable '%s' is not defined" name)
      | Some false ->
          failwith
            (Printf.sprintf
               "Cannot assign to immutable variable '%s'. Use 'let mut' \
                instead."
               name)
      | Some true -> ());
      compile_expr ctx expr;
      emit ctx (Assign name)
  | Ast.Return e ->
      compile_expr ctx e;
      emit ctx Return
  | Ast.Block body -> List.iter (compile_stmt ctx) body
  | Ast.For body -> List.iter (compile_stmt ctx) body
  | Ast.Break -> emit ctx Break
  | Ast.Import _ -> ()

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
