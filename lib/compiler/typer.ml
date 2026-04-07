open Ce_parser.Ast

type env = {
  vars : (string, types) Hashtbl.t;
  funcs : (string, types list * types) Hashtbl.t; 
}

let env_create () = {
  vars = Hashtbl.create 10;
  funcs = Hashtbl.create 10;
}

let rec check_expr env (e : expr) : expr =
  match e.kind with
  | Int _ -> { e with ty = TypeInt }
  | Float _ -> { e with ty = TypeFloat }
  | String _ -> { e with ty = TypeString }
  | Bool _ -> { e with ty = TypeBool }

  | Let id ->
      (match Hashtbl.find_opt env.vars id with
      | Some t -> { e with ty = t }
      | None -> failwith ("Type Error: Undefined variable '" ^ id ^ "'"))

  | Add (l, r) ->
      let l_typed = check_expr env l in
      let r_typed = check_expr env r in
     
      if l_typed.ty = TypeInt && r_typed.ty = TypeInt then
        { kind = Add (l_typed, r_typed); ty = TypeInt }
      else if l_typed.ty = TypeFloat && r_typed.ty = TypeFloat then
        { kind = Add (l_typed, r_typed); ty = TypeFloat }
      else
        failwith "Type Error: Cannot add mismatched or non-numeric types"
  | Sub (l, r) ->
      let l_typed = check_expr env l in
      let r_typed = check_expr env r in
      if l_typed.ty = TypeInt && r_typed.ty = TypeInt then
        { kind = Sub (l_typed, r_typed); ty = TypeInt }
      else if l_typed.ty = TypeFloat && r_typed.ty = TypeFloat then
        { kind = Sub (l_typed, r_typed); ty = TypeFloat }
      else failwith "Type Error: Cannot subtract mismatched or non-numeric types"

  | Mul (l, r) ->
      let l_typed = check_expr env l in
      let r_typed = check_expr env r in
      if l_typed.ty = TypeInt && r_typed.ty = TypeInt then
        { kind = Mul (l_typed, r_typed); ty = TypeInt }
      else if l_typed.ty = TypeFloat && r_typed.ty = TypeFloat then
        { kind = Mul (l_typed, r_typed); ty = TypeFloat }
      else failwith "Type Error: Cannot multiply mismatched or non-numeric types"

  | Div (l, r) ->
      let l_typed = check_expr env l in
      let r_typed = check_expr env r in
      if l_typed.ty = TypeInt && r_typed.ty = TypeInt then
        { kind = Div (l_typed, r_typed); ty = TypeInt }
      else if l_typed.ty = TypeFloat && r_typed.ty = TypeFloat then
        { kind = Div (l_typed, r_typed); ty = TypeFloat }
      else failwith "Type Error: Cannot divide mismatched or non-numeric types"

  | Mod (l, r) ->
      let l_typed = check_expr env l in
      let r_typed = check_expr env r in
      if l_typed.ty = TypeInt && r_typed.ty = TypeInt then
        { kind = Mod (l_typed, r_typed); ty = TypeInt }
      else failwith "Type Error: Modulo operator (%) requires integer types"

  | Call (fn_name, args) ->
      let args_typed = List.map (check_expr env) args in
      let arg_types = List.map (fun (a : expr) -> a.ty) args_typed in
      
      if fn_name = "println" then
        { kind = Call (fn_name, args_typed); ty = TypeVoid }
      else if fn_name = "print" then
        { kind = Call (fn_name, args_typed); ty = TypeVoid }
      else
        (match Hashtbl.find_opt env.funcs fn_name with
        | Some (param_tys, ret_ty) ->
            if param_tys <> arg_types then 
              failwith ("Type Error: Argument mismatch in call to " ^ fn_name);
            { kind = Call (fn_name, args_typed); ty = ret_ty }
        | None -> failwith ("Type Error: Undefined function '" ^ fn_name ^ "'"))

  | _ -> e 

let rec check_stmt env (s : stmt) : stmt =
  match s with
  | Expr e -> 
      Expr (check_expr env e)
      
  | DefLet (name, is_mut, declared_ty, expr) ->
      let expr_typed = check_expr env expr in
      if expr_typed.ty <> declared_ty then
        failwith (Printf.sprintf "Type Error: Variable '%s' declared as %s but assigned %s" 
                  name (show_types declared_ty) (show_types expr_typed.ty));
      
      Hashtbl.add env.vars name declared_ty;
      DefLet (name, is_mut, declared_ty, expr_typed)

  | Assign (name, expr) ->
      let expr_typed = check_expr env expr in
      let var_ty = Hashtbl.find env.vars name in
      if expr_typed.ty <> var_ty then
        failwith ("Type Error: Invalid assignment to '" ^ name ^ "'");
      Assign (name, expr_typed)

  | Block stmts ->
      let block_env = {
        vars = Hashtbl.copy env.vars;
        funcs = Hashtbl.copy env.funcs;
      } in
      let stmts_typed = List.map (check_stmt block_env) stmts in
      Block stmts_typed

  | DefFN (name, params, ty, body) ->
      let param_tys = List.map (fun p -> p.ty) params in
      Hashtbl.add env.funcs name (param_tys, ty);
      let fn_env = { 
        vars = Hashtbl.copy env.vars; 
        funcs = Hashtbl.copy env.funcs 
      } in
      List.iter (fun p -> Hashtbl.add fn_env.vars p.name p.ty) params;
      let typed_body = List.map (check_stmt fn_env) body in
      DefFN (name, params, ty, typed_body)

  | Return e ->
      let typed_expr = check_expr env e in
      Return typed_expr
  | _ -> s

let check_program (prog : stmt list) : stmt list =
  let global_env = env_create () in
  List.map (check_stmt global_env) prog
