open Llvm
open Ce_parser.Ast

exception Error of string

let type_aliases : (string, types) Hashtbl.t = Hashtbl.create 10

let named_values : (string, llvalue * types * bool) Hashtbl.t =
  Hashtbl.create 10

let function_types : (string, lltype) Hashtbl.t = Hashtbl.create 10

let struct_templates :
    (string, (string * types) list * struct_field list) Hashtbl.t =
  Hashtbl.create 10

let impl_templates :
    ( string,
      (string * types) list
      * (string * string * param list * types * stmt list) list )
    Hashtbl.t =
  Hashtbl.create 10

let struct_registry : (string, lltype * (string * int) list) Hashtbl.t =
  Hashtbl.create 10

let loop_exit_blocks : llbasicblock Stack.t = Stack.create ()
let current_fn_is_res = ref false
let current_fn_ret_ty = ref (void_type ce_ctx)

let rec substitute_type type_map = function
  | TGenericParam name -> (
      try List.assoc name type_map with Not_found -> TGenericParam name)
  | TNamed name -> (
      try List.assoc name type_map with Not_found -> TNamed name)
  | TPointer ty -> TPointer (substitute_type type_map ty)
  | TArray (n, ty) -> TArray (n, substitute_type type_map ty)
  | TGenericInst (name, args) ->
      TGenericInst (name, List.map (substitute_type type_map) args)
  | TResult ty -> TResult (substitute_type type_map ty)
  | other -> other

and substitute_expr type_map = function
  | Array (n, ty, elems) ->
      Array
        ( n,
          substitute_type type_map ty,
          List.map (substitute_expr type_map) elems )
  | Struct (name, targs, fields) ->
      let new_targs = List.map (substitute_type type_map) targs in
      let new_fields =
        List.map (fun (n, e) -> (n, substitute_expr type_map e)) fields
      in
      Struct (name, new_targs, new_fields)
  | Add (l, r) -> Add (substitute_expr type_map l, substitute_expr type_map r)
  | Sub (l, r) -> Sub (substitute_expr type_map l, substitute_expr type_map r)
  | Mul (l, r) -> Mul (substitute_expr type_map l, substitute_expr type_map r)
  | Div (l, r) -> Div (substitute_expr type_map l, substitute_expr type_map r)
  | Mod (l, r) -> Mod (substitute_expr type_map l, substitute_expr type_map r)
  | Eq (l, r) -> Eq (substitute_expr type_map l, substitute_expr type_map r)
  | Lt (l, r) -> Lt (substitute_expr type_map l, substitute_expr type_map r)
  | Lte (l, r) -> Lte (substitute_expr type_map l, substitute_expr type_map r)
  | Gt (l, r) -> Gt (substitute_expr type_map l, substitute_expr type_map r)
  | Gte (l, r) -> Gte (substitute_expr type_map l, substitute_expr type_map r)
  | And (l, r) -> And (substitute_expr type_map l, substitute_expr type_map r)
  | Or (l, r) -> Or (substitute_expr type_map l, substitute_expr type_map r)
  | Neg e -> Neg (substitute_expr type_map e)
  | Ref e -> Ref (substitute_expr type_map e)
  | Deref e -> Deref (substitute_expr type_map e)
  | Call (name, args) -> Call (name, List.map (substitute_expr type_map) args)
  | ArrayAccess (name, idx) -> ArrayAccess (name, substitute_expr type_map idx)
  | If (cond, then_b, elifs, else_b) ->
      let s_cond = substitute_expr type_map cond in
      let s_then = List.map (substitute_stmt type_map) then_b in
      let s_elifs =
        List.map
          (fun (c, b) ->
            (substitute_expr type_map c, List.map (substitute_stmt type_map) b))
          elifs
      in
      let s_else =
        match else_b with
        | None -> None
        | Some b -> Some (List.map (substitute_stmt type_map) b)
      in
      If (s_cond, s_then, s_elifs, s_else)
  | Catch (e, id, ty, stmts) ->
      Catch
        ( substitute_expr type_map e,
          id,
          substitute_type type_map ty,
          List.map (substitute_stmt type_map) stmts )
  | e -> e

and substitute_stmt type_map = function
  | Expr e -> Expr (substitute_expr type_map e)
  | DefLet (name, is_mut, ty, e) ->
      let new_e =
        match e with
        | Some ee -> Some (substitute_expr type_map ee)
        | None -> None
      in
      DefLet (name, is_mut, substitute_type type_map ty, new_e)
  | Assign (name, e) -> Assign (name, substitute_expr type_map e)
  | ArrayAssign (name, idx, e) ->
      ArrayAssign
        (name, substitute_expr type_map idx, substitute_expr type_map e)
  | DerefAssign (ptr, e) ->
      DerefAssign (substitute_expr type_map ptr, substitute_expr type_map e)
  | Return e -> Return (substitute_expr type_map e)
  | Block stmts -> Block (List.map (substitute_stmt type_map) stmts)
  | For stmts -> For (List.map (substitute_stmt type_map) stmts)
  | DefFN (name, params, ret_ty, body) ->
      let s_params =
        List.map
          (fun p ->
            { param_name = p.param_name; ty = substitute_type type_map p.ty })
          params
      in
      DefFN
        ( name,
          s_params,
          substitute_type type_map ret_ty,
          List.map (substitute_stmt type_map) body )
  | Raise e -> Raise (substitute_expr type_map e)
  | s -> s

let rec llvm_type_of = function
  | TInt -> i64_type ce_ctx
  | TFloat -> double_type ce_ctx
  | TBool -> i1_type ce_ctx
  | TVoid -> void_type ce_ctx
  | TString -> pointer_type ce_ctx
  | TChar -> i8_type ce_ctx
  | TPointer _ -> pointer_type ce_ctx
  | TArray (n, ty) -> array_type (llvm_type_of ty) n
  | TNamed name -> (
      match Hashtbl.find_opt type_aliases name with
      | Some actual_ty -> llvm_type_of actual_ty
      | None -> (
          match Hashtbl.find_opt struct_registry name with
          | Some (llty, _) -> llty
          | None -> raise (Error ("Undefined type: " ^ name))))
  | TStruct name ->
      let llty, _ = Hashtbl.find struct_registry name in
      llty
  | TUnknown -> raise (Error "Cannot compile unknown type")
  | TGenericParam name ->
      raise (Error ("Uninstantiated generic parameter '" ^ name))
  | TResult ty ->
      let inner = llvm_type_of ty in
      let ok_ty = if inner = void_type ce_ctx then i1_type ce_ctx else inner in
      struct_type ce_ctx [| i1_type ce_ctx; ok_ty; pointer_type ce_ctx |]
  | TGenericInst (name, arg_types) -> (
      let mangled_name =
        name ^ "_" ^ String.concat "_" (List.map show_types arg_types)
      in
      match Hashtbl.find_opt struct_registry mangled_name with
      | Some (llty, _) -> llty
      | None ->
          let saved_bb =
            try Some (insertion_block ce_builder) with Not_found -> None
          in

          let params, fields = Hashtbl.find struct_templates name in
          let type_map =
            List.map2
              (fun (p_name, _) arg_ty -> (p_name, arg_ty))
              params arg_types
          in

          let specialized_fields =
            List.map
              (fun f ->
                {
                  field_name = f.field_name;
                  ty = substitute_type type_map f.ty;
                })
              fields
          in

          ignore
            (codegen_stmt (DefStruct (mangled_name, [], specialized_fields)));

          (match Hashtbl.find_opt impl_templates name with
          | Some (_, methods) ->
              let specialized_methods =
                List.map
                  (fun (m_name, self_id, m_params, ret_ty, body) ->
                    let sub_params =
                      List.map
                        (fun p ->
                          {
                            param_name = p.param_name;
                            ty = substitute_type type_map p.ty;
                          })
                        m_params
                    in
                    let sub_body = List.map (substitute_stmt type_map) body in
                    ( m_name,
                      self_id,
                      sub_params,
                      substitute_type type_map ret_ty,
                      sub_body ))
                  methods
              in
              ignore
                (codegen_stmt (Impl (mangled_name, [], specialized_methods)))
          | None -> ());

          (match saved_bb with
          | Some bb -> position_at_end bb ce_builder
          | None -> ());

          fst (Hashtbl.find struct_registry mangled_name))

and codegen_expr = function
  | Void -> const_null (void_type ce_ctx)
  | Int n -> const_int (i64_type ce_ctx) n
  | Float f -> const_float (double_type ce_ctx) f
  | Bool b -> const_int (i1_type ce_ctx) (if b then 1 else 0)
  | Char c -> const_int (i8_type ce_ctx) (Char.code c)
  | String s -> build_global_stringptr s "strtmp" ce_builder
  | ArrayAccess (name, index_expr) ->
      let array_ptr_val, array_ty =
        match Hashtbl.find_opt named_values name with
        | Some (v, ty, _) -> (v, ty)
        | None -> raise (Error ("Array '" ^ name ^ "' not found"))
      in
      let llvm_array_ty = llvm_type_of array_ty in

      let idx_val = codegen_expr index_expr in
      let zero = const_int (i32_type ce_ctx) 0 in
      let indices = [| zero; idx_val |] in
      let element_ptr =
        build_in_bounds_gep llvm_array_ty array_ptr_val indices "arrayidx"
          ce_builder
      in
      let elem_ty = element_type llvm_array_ty in
      build_load elem_ty element_ptr "loadtmp" ce_builder
  | Let name -> (
      if String.contains name '.' then
        let parts = String.split_on_char '.' name in
        let base_name = List.hd parts in
        let props = List.tl parts in

        match Hashtbl.find_opt named_values base_name with
        | Some (v, ast_ty, _) ->
            let base_val =
              build_load (llvm_type_of ast_ty) v base_name ce_builder
            in
            let rec extract current_val current_ty props =
              match props with
              | [] -> current_val
              | prop :: rest -> (
                  match classify_type current_ty with
                  | TypeKind.Struct -> (
                      let struct_name_opt = struct_name current_ty in
                      match struct_name_opt with
                      | Some s_name -> (
                          let clean_name =
                            if String.starts_with ~prefix:"struct." s_name then
                              String.sub s_name 7 (String.length s_name - 7)
                            else s_name
                          in
                          match Hashtbl.find_opt struct_registry clean_name with
                          | Some (_, field_map) -> (
                              try
                                let idx = List.assoc prop field_map in
                                let next_val =
                                  build_extractvalue current_val idx "proptmp"
                                    ce_builder
                                in
                                extract next_val (type_of next_val) rest
                              with Not_found ->
                                raise
                                  (Error
                                     ("Unknown property '" ^ prop
                                    ^ "' on struct '" ^ clean_name ^ "'")))
                          | None ->
                              raise
                                (Error
                                   ("Could not find struct definition for '"
                                  ^ clean_name ^ "'")))
                      | None ->
                          raise
                            (Error
                               "Cannot access property on an anonymous struct"))
                  | _ ->
                      raise
                        (Error
                           ("Cannot access property '" ^ prop
                          ^ "' on non-struct type")))
            in
            extract base_val (type_of base_val) props
        | None -> raise (Error ("Unknown variable: " ^ base_name))
      else
        match Hashtbl.find_opt named_values name with
        | Some (v, ast_ty, _) ->
            build_load (llvm_type_of ast_ty) v name ce_builder
        | None -> raise (Error ("Unknown variable: " ^ name)))
  | Ref (Let name) ->
      let ptr_val, _, _ = Hashtbl.find named_values name in
      ptr_val
  | Ref _ -> raise (Error "Can only reference variables (e.g., &a)")
  | Deref (Let name) ->
      let ptr_to_ptr, ast_ty, _ = Hashtbl.find named_values name in
      let inner_ty =
        match ast_ty with
        | TPointer t -> t
        | _ -> raise (Error ("Variable '" ^ name ^ "' is not a pointer"))
      in
      let actual_ptr =
        build_load (llvm_type_of ast_ty) ptr_to_ptr "ptrload" ce_builder
      in
      build_load (llvm_type_of inner_ty) actual_ptr "dereftmp" ce_builder
  | Deref _ -> raise (Error "Complex pointer math not yet supported")
  | Add (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      let ty_l = classify_type (type_of lv) in
      let ty_r = classify_type (type_of rv) in

      if ty_l = TypeKind.Pointer && ty_r = TypeKind.Integer then
        build_in_bounds_gep
          (element_type (type_of lv))
          lv [| rv |] "ptraddtmp" ce_builder
      else if ty_l = TypeKind.Integer && ty_r = TypeKind.Pointer then
        build_in_bounds_gep
          (element_type (type_of rv))
          rv [| lv |] "ptraddtmp" ce_builder
      else if type_of lv = double_type ce_ctx then
        build_fadd lv rv "faddtmp" ce_builder
      else build_add lv rv "addtmp" ce_builder
  | Sub (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      let ty_l = classify_type (type_of lv) in
      let ty_r = classify_type (type_of rv) in

      if ty_l = TypeKind.Pointer && ty_r = TypeKind.Integer then
        let neg_rv = build_neg rv "negidx" ce_builder in
        build_in_bounds_gep
          (element_type (type_of lv))
          lv [| neg_rv |] "ptrsubtmp" ce_builder
      else if type_of lv = double_type ce_ctx then
        build_fsub lv rv "fsubtmp" ce_builder
      else build_sub lv rv "subtmp" ce_builder
  | Mul (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
        build_fmul lv rv "fmultmp" ce_builder
      else build_mul lv rv "multmp" ce_builder
  | Div (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
        build_fdiv lv rv "fdivtmp" ce_builder
      else build_sdiv lv rv "divtmp" ce_builder
  | Mod (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
        build_frem lv rv "fmodtmp" ce_builder
      else build_srem lv rv "modtmp" ce_builder
  | Eq (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
        build_fcmp Fcmp.Oeq lv rv "feqtmp" ce_builder
      else build_icmp Icmp.Eq lv rv "eqtmp" ce_builder
  | Lt (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
        build_fcmp Fcmp.Olt lv rv "flttmp" ce_builder
      else build_icmp Icmp.Slt lv rv "lttmp" ce_builder
  | Lte (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
        build_fcmp Fcmp.Ole lv rv "fltetmp" ce_builder
      else build_icmp Icmp.Sle lv rv "ltetmp" ce_builder
  | Gt (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
        build_fcmp Fcmp.Ogt lv rv "fgttmp" ce_builder
      else build_icmp Icmp.Sgt lv rv "gttmp" ce_builder
  | Gte (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
        build_fcmp Fcmp.Oge lv rv "fgtetmp" ce_builder
      else build_icmp Icmp.Sge lv rv "gtetmp" ce_builder
  | And (l, r) ->
      build_and (codegen_expr l) (codegen_expr r) "andtmp" ce_builder
  | Or (l, r) -> build_or (codegen_expr l) (codegen_expr r) "ortmp" ce_builder
  | Neg e ->
      let v = codegen_expr e in
      if type_of v = double_type ce_ctx then build_fneg v "fnegtmp" ce_builder
      else build_neg v "negtmp" ce_builder
  | Call (name, args) -> (
      if String.contains name '.' then
        let last_dot_idx = String.rindex name '.' in
        let base_path = String.sub name 0 last_dot_idx in
        let method_name =
          String.sub name (last_dot_idx + 1)
            (String.length name - last_dot_idx - 1)
        in
        if Hashtbl.mem struct_registry base_path then
          let mangled_name = base_path ^ "::" ^ method_name in
          let callee =
            match lookup_function mangled_name ce_module with
            | Some c -> c
            | None ->
                raise
                  (Error
                     ("Unknown method '" ^ method_name ^ "' on struct '"
                    ^ base_path ^ "'"))
          in
          let ft =
            match Hashtbl.find_opt function_types mangled_name with
            | Some t -> t
            | None ->
                raise
                  (Error ("Unknown function type for method: " ^ mangled_name))
          in
          let args_val = Array.of_list (List.map codegen_expr args) in
          build_call ft callee args_val "staticcalltmp" ce_builder
        else
          let self_val = codegen_expr (Let base_path) in
          let self_ty_llvm = type_of self_val in

          match struct_name self_ty_llvm with
          | Some s_name ->
              let clean_name =
                if String.starts_with ~prefix:"struct." s_name then
                  String.sub s_name 7 (String.length s_name - 7)
                else s_name
              in

              let mangled_name = clean_name ^ "::" ^ method_name in
              let callee =
                match lookup_function mangled_name ce_module with
                | Some c -> c
                | None ->
                    raise
                      (Error
                         ("Unknown method '" ^ method_name ^ "' on struct '"
                        ^ clean_name ^ "'"))
              in

              let ft =
                match Hashtbl.find_opt function_types mangled_name with
                | Some t -> t
                | None ->
                    raise
                      (Error
                         ("Unknown function type for method: " ^ mangled_name))
              in
              let compiled_args = List.map codegen_expr args in
              let all_args = Array.of_list (self_val :: compiled_args) in

              build_call ft callee all_args "methodcalltmp" ce_builder
          | None ->
              raise
                (Error
                   ("Cannot call method '" ^ method_name
                  ^ "' on a non-struct type"))
      else
        let arg_vals = List.map codegen_expr args in
        match Builtin.get name with
        | Some builtin_fn ->
            builtin_fn ce_ctx ce_module ce_builder name arg_vals
        | None ->
            let callee =
              match lookup_function name ce_module with
              | Some f -> f
              | None -> raise (Error ("Unknown function: " ^ name))
            in
            let ft =
              match Hashtbl.find_opt function_types name with
              | Some t -> t
              | None -> raise (Error ("Unknown function type for: " ^ name))
            in
            let args_val = Array.of_list arg_vals in

            build_call ft callee args_val "calltmp" ce_builder)
  | Array (n, ty, elems) ->
      let elem_ty = llvm_type_of ty in
      let arr_ty = array_type elem_ty n in
      let arr_alloc = build_alloca arr_ty "arrtmp" ce_builder in
      List.iteri
        (fun i e ->
          let e_val = codegen_expr e in
          let idx =
            [| const_int (i32_type ce_ctx) 0; const_int (i32_type ce_ctx) i |]
          in
          let ptr = build_gep arr_ty arr_alloc idx "elemtmp" ce_builder in
          ignore (build_store e_val ptr ce_builder))
        elems;
      build_load arr_ty arr_alloc "arrload" ce_builder
  | If (cond, then_body, elif_branches, else_body) ->
      let the_function = block_parent (insertion_block ce_builder) in
      let merge_bb = append_block ce_ctx "ifcont" the_function in
      let codegen_block_yield stmts =
        let rec aux = function
          | [] -> const_null (void_type ce_ctx)
          | [ Expr e ] -> codegen_expr e
          | s :: rest ->
              ignore (codegen_stmt s);
              aux rest
        in
        aux stmts
      in

      let phi_incoming = ref [] in
      let rec build_if c body rest_elifs else_b =
        let cond_val = codegen_expr c in
        let then_bb = append_block ce_ctx "then" the_function in
        let next_bb = append_block ce_ctx "else_or_elif" the_function in

        ignore (build_cond_br cond_val then_bb next_bb ce_builder);
        position_at_end then_bb ce_builder;

        let then_val = codegen_block_yield body in
        let then_bb_end = insertion_block ce_builder in
        (match block_terminator then_bb_end with
        | None ->
            ignore (build_br merge_bb ce_builder);
            phi_incoming := (then_val, then_bb_end) :: !phi_incoming
        | Some _ -> ());
        position_at_end next_bb ce_builder;
        match rest_elifs with
        | (elif_c, elif_body) :: rest -> build_if elif_c elif_body rest else_b
        | [] -> (
            let else_val =
              match else_b with
              | Some stmts -> codegen_block_yield stmts
              | None -> const_null (void_type ce_ctx)
            in
            let else_bb_end = insertion_block ce_builder in

            match block_terminator else_bb_end with
            | None ->
                ignore (build_br merge_bb ce_builder);
                phi_incoming := (else_val, else_bb_end) :: !phi_incoming
            | Some _ -> ())
      in
      build_if cond then_body elif_branches else_body;
      position_at_end merge_bb ce_builder;

      let incoming = List.rev !phi_incoming in
      if incoming = [] then const_null (void_type ce_ctx)
      else
        let first_val, _ = List.hd incoming in
        let ty = type_of first_val in
        if ty = void_type ce_ctx then const_null (void_type ce_ctx)
        else build_phi incoming "iftmp" ce_builder
  | Struct (name, type_args, fields) ->
      if type_args <> [] then begin
        ignore (llvm_type_of (TGenericInst (name, type_args)))
      end;

      let mangled_name =
        if type_args = [] then name
        else name ^ "_" ^ String.concat "_" (List.map show_types type_args)
      in
      let llty, field_map = Hashtbl.find struct_registry mangled_name in
      let alloc = build_alloca llty "structtmp" ce_builder in

      List.iter
        (fun (fname, fexpr) ->
          let fidx = List.assoc fname field_map in
          let fptr = build_struct_gep llty alloc fidx "fieldptr" ce_builder in
          ignore (build_store (codegen_expr fexpr) fptr ce_builder))
        fields;

      build_load llty alloc "structload" ce_builder
  | Catch (expr, err_name, catch_ty, body) ->
      let res_val = codegen_expr expr in
      let is_err = build_extractvalue res_val 0 "is_err" ce_builder in

      let the_func = block_parent (insertion_block ce_builder) in
      let err_bb = append_block ce_ctx "catch_err" the_func in
      let ok_bb = append_block ce_ctx "catch_ok" the_func in
      let merge_bb = append_block ce_ctx "catch_merge" the_func in

      ignore (build_cond_br is_err err_bb ok_bb ce_builder);

      position_at_end err_bb ce_builder;
      let err_str = build_extractvalue res_val 2 "err_str" ce_builder in
      let err_alloc = build_alloca (pointer_type ce_ctx) err_name ce_builder in
      ignore (build_store err_str err_alloc ce_builder);

      let old_val_opt = Hashtbl.find_opt named_values err_name in
      Hashtbl.add named_values err_name (err_alloc, TString, false);

      let catch_ty_ll = llvm_type_of catch_ty in
      let catch_val = ref (const_null catch_ty_ll) in

      List.iter
        (fun s ->
          match s with
          | Return e -> catch_val := codegen_expr e
          | _ -> ignore (codegen_stmt s))
        body;

      Hashtbl.remove named_values err_name;
      (match old_val_opt with
      | Some v -> Hashtbl.add named_values err_name v
      | None -> ());

      let err_end_bb = insertion_block ce_builder in
      let err_has_term =
        match block_terminator err_end_bb with None -> false | Some _ -> true
      in
      if not err_has_term then ignore (build_br merge_bb ce_builder);

      position_at_end ok_bb ce_builder;
      let ok_val =
        if catch_ty_ll = void_type ce_ctx then const_null (void_type ce_ctx)
        else build_extractvalue res_val 1 "ok_val" ce_builder
      in
      let ok_end_bb = insertion_block ce_builder in
      ignore (build_br merge_bb ce_builder);

      position_at_end merge_bb ce_builder;
      if catch_ty_ll = void_type ce_ctx then const_null (void_type ce_ctx)
      else if not err_has_term then
        build_phi
          [ (!catch_val, err_end_bb); (ok_val, ok_end_bb) ]
          "catch_res" ce_builder
      else ok_val

and codegen_stmt = function
  | Expr e ->
      ignore (codegen_expr e);
      const_null (void_type ce_ctx)
  | DefLet (name, ismut, ty, expr_opt) ->
      let ll_ty = llvm_type_of ty in
      let init_val =
        match expr_opt with
        | Some expr -> codegen_expr expr
        | None -> const_null ll_ty
      in
      let the_function = block_parent (insertion_block ce_builder) in
      let ce_builder_alloca =
        builder_at ce_ctx (instr_begin (entry_block the_function))
      in

      let alloca = build_alloca ll_ty name ce_builder_alloca in
      ignore (build_store init_val alloca ce_builder);

      Hashtbl.add named_values name (alloca, ty, ismut);
      alloca
  | DefType (name, underlying_ty) ->
      Hashtbl.add type_aliases name underlying_ty;
      const_null (void_type ce_ctx)
  | DefStruct (name, params, fields) ->
      if List.length params > 0 then begin
        Hashtbl.add struct_templates name (params, fields);
        const_null (void_type ce_ctx)
      end
      else begin
        let field_types =
          Array.of_list (List.map (fun f -> llvm_type_of f.ty) fields)
        in
        let struct_llty = named_struct_type ce_ctx name in
        struct_set_body struct_llty field_types false;

        let field_map = List.mapi (fun i f -> (f.field_name, i)) fields in
        Hashtbl.add struct_registry name (struct_llty, field_map);
        const_null (void_type ce_ctx)
      end
  | Assign (name, expr) ->
      let val_ = codegen_expr expr in
      let var_ptr =
        if String.contains name '.' then
          let parts = String.split_on_char '.' name in
          let base_name = List.hd parts in
          let props = List.tl parts in

          match Hashtbl.find_opt named_values base_name with
          | Some (v, ast_ty, ismut) ->
              if not ismut then
                raise
                  (Error
                     ("Cannot assign to property of immutable variable '"
                    ^ base_name ^ "'"));

              let rec get_gep current_ptr current_ty props =
                match props with
                | [] -> current_ptr
                | prop :: rest -> (
                    match classify_type current_ty with
                    | TypeKind.Struct -> (
                        let struct_name_opt = struct_name current_ty in
                        match struct_name_opt with
                        | Some s_name -> (
                            let clean_name =
                              if String.starts_with ~prefix:"struct." s_name
                              then String.sub s_name 7 (String.length s_name - 7)
                              else s_name
                            in
                            match
                              Hashtbl.find_opt struct_registry clean_name
                            with
                            | Some (_, field_map) -> (
                                try
                                  let idx = List.assoc prop field_map in
                                  let next_ptr =
                                    build_struct_gep current_ty current_ptr idx
                                      "prop_ptr" ce_builder
                                  in
                                  let struct_elem_types =
                                    struct_element_types current_ty
                                  in
                                  let next_ty = struct_elem_types.(idx) in
                                  get_gep next_ptr next_ty rest
                                with Not_found ->
                                  raise
                                    (Error
                                       ("Unknown property '" ^ prop
                                      ^ "' on struct '" ^ clean_name ^ "'")))
                            | None ->
                                raise
                                  (Error
                                     ("Could not find struct definition for '"
                                    ^ clean_name ^ "'")))
                        | None ->
                            raise
                              (Error
                                 "Cannot access property on an anonymous struct")
                        )
                    | _ ->
                        raise
                          (Error
                             ("Cannot access property '" ^ prop
                            ^ "' on non-struct type")))
              in
              get_gep v (llvm_type_of ast_ty) props
          | None ->
              raise
                (Error ("Variable not defined for assignment: " ^ base_name))
        else
          match Hashtbl.find_opt named_values name with
          | Some (v, _, ismut) ->
              if not ismut then
                raise
                  (Error ("Cannot assign to immutable variable '" ^ name ^ "'"));
              v
          | None ->
              raise (Error ("Variable not defined for assignment: " ^ name))
      in
      ignore (build_store val_ var_ptr ce_builder);
      val_
  | ArrayAssign (name, index_expr, val_expr) ->
      let array_ptr_val, array_ty =
        match Hashtbl.find_opt named_values name with
        | Some (v, ty, ismut) ->
            if not ismut then
              raise (Error ("Cannot assign to immutable array '" ^ name ^ "'"));
            (v, ty)
        | None ->
            raise (Error ("Array '" ^ name ^ "' not found for assignment"))
      in

      let idx_val = codegen_expr index_expr in
      let val_to_store = codegen_expr val_expr in

      let zero = const_int (i32_type ce_ctx) 0 in
      let indices = [| zero; idx_val |] in
      let element_ptr =
        build_in_bounds_gep (llvm_type_of array_ty) array_ptr_val indices
          "arrayidx" ce_builder
      in

      ignore (build_store val_to_store element_ptr ce_builder);
      val_to_store
  | DerefAssign (Let name, val_expr) ->
      let ptr_to_ptr, ast_ty, _ = Hashtbl.find named_values name in
      let _ =
        match ast_ty with
        | TPointer t -> t
        | _ -> raise (Error ("Variable '" ^ name ^ "' is not a pointer"))
      in
      let actual_ptr =
        build_load (llvm_type_of ast_ty) ptr_to_ptr "ptrload" ce_builder
      in
      let val_to_store = codegen_expr val_expr in
      ignore (build_store val_to_store actual_ptr ce_builder);
      val_to_store
  | DerefAssign _ -> raise (Error "Can only assign to variable pointers")
  | DefFN (name, params, ret_ty, body) ->
      let actual_name = if name = "main" then "__ce_main" else name in

      let was_res = !current_fn_is_res in
      let was_ret_ty = !current_fn_ret_ty in

      (current_fn_is_res := match ret_ty with TResult _ -> true | _ -> false);
      current_fn_ret_ty := llvm_type_of ret_ty;

      let param_types =
        Array.of_list (List.map (fun (p : param) -> llvm_type_of p.ty) params)
      in
      let ft = function_type (llvm_type_of ret_ty) param_types in
      Hashtbl.add function_types actual_name ft;

      let f = declare_function actual_name ft ce_module in
      let bb = append_block ce_ctx "entry" f in
      position_at_end bb ce_builder;

      let old_named_values = Hashtbl.copy named_values in
      Array.iteri
        (fun i a ->
          let n = (List.nth params i).param_name in
          let p_ty = (List.nth params i).ty in
          let llvm_p_ty = llvm_type_of p_ty in
          let alloca = build_alloca llvm_p_ty n ce_builder in
          ignore (build_store a alloca ce_builder);
          Hashtbl.add named_values n (alloca, p_ty, false))
        (Llvm.params f);

      List.iter (fun s -> ignore (codegen_stmt s)) body;

      let current_bb = insertion_block ce_builder in
      (match block_terminator current_bb with
      | Some _ -> ()
      | None ->
          if ret_ty = TVoid then ignore (build_ret_void ce_builder)
          else if !current_fn_is_res then begin
            let r_ty = !current_fn_ret_ty in
            let s1 =
              build_insertvalue (const_null r_ty)
                (const_int (i1_type ce_ctx) 0)
                0 "res_ok" ce_builder
            in
            ignore (build_ret s1 ce_builder)
          end
          else
            raise
              (Error ("Function '" ^ name ^ "' is missing a return statement")));

      Hashtbl.clear named_values;
      Hashtbl.iter (fun k v -> Hashtbl.add named_values k v) old_named_values;

      current_fn_is_res := was_res;
      current_fn_ret_ty := was_ret_ty;

      if name = "main" then begin
        let c_main_ty = function_type (i32_type ce_ctx) [||] in
        let c_main_f = declare_function "main" c_main_ty ce_module in
        let c_bb = append_block ce_ctx "entry" c_main_f in
        let c_builder = builder_at_end ce_ctx c_bb in

        let call_res = build_call ft f [||] "main_call" c_builder in

        if match ret_ty with TResult _ -> true | _ -> false then begin
          let is_err = build_extractvalue call_res 0 "is_err" c_builder in
          let err_bb = append_block ce_ctx "err" c_main_f in
          let ok_bb = append_block ce_ctx "ok" c_main_f in

          ignore (build_cond_br is_err err_bb ok_bb c_builder);

          position_at_end err_bb c_builder;
          let err_msg = build_extractvalue call_res 2 "err_msg" c_builder in
          let printf_f = Builtin.get_printf ce_ctx ce_module in
          let fmt_str =
            build_global_stringptr "Uncaught Error: %s\n" "err_fmt" c_builder
          in
          ignore
            (build_call
               (var_arg_function_type (i32_type ce_ctx)
                  [| pointer_type ce_ctx |])
               printf_f [| fmt_str; err_msg |] "printf_call" c_builder);
          ignore (build_ret (const_int (i32_type ce_ctx) 1) c_builder);

          position_at_end ok_bb c_builder;
          ignore (build_ret (const_int (i32_type ce_ctx) 0) c_builder)
        end
        else begin
          ignore (build_ret (const_int (i32_type ce_ctx) 0) c_builder)
        end
      end;
      f
  | Block stmts ->
      List.iter (fun s -> ignore (codegen_stmt s)) stmts;
      const_null (void_type ce_ctx)
  | For stmts ->
      let the_function = block_parent (insertion_block ce_builder) in
      let loop_bb = append_block ce_ctx "loop" the_function in
      let after_bb = append_block ce_ctx "afterloop" the_function in

      ignore (build_br loop_bb ce_builder);
      position_at_end loop_bb ce_builder;

      Stack.push after_bb loop_exit_blocks;
      List.iter (fun s -> ignore (codegen_stmt s)) stmts;
      ignore (build_br loop_bb ce_builder);
      ignore (Stack.pop loop_exit_blocks);

      position_at_end after_bb ce_builder;
      const_null (void_type ce_ctx)
  | Break ->
      if Stack.is_empty loop_exit_blocks then
        raise (Error "Break outside of a loop");
      let exit_block = Stack.top loop_exit_blocks in
      ignore (build_br exit_block ce_builder);
      const_null (void_type ce_ctx)
  | Return e ->
      let v = codegen_expr e in
      if !current_fn_is_res then begin
        let ret_ty = !current_fn_ret_ty in
        let s1 =
          build_insertvalue (const_null ret_ty)
            (const_int (i1_type ce_ctx) 0)
            0 "ok_flag" ce_builder
        in
        let s2 =
          if type_of v = void_type ce_ctx then s1
          else build_insertvalue s1 v 1 "ok_val" ce_builder
        in
        ignore (build_ret s2 ce_builder);
        const_null (void_type ce_ctx)
      end
      else begin
        ignore (build_ret v ce_builder);
        const_null (void_type ce_ctx)
      end
  | Import _ -> const_null (void_type ce_ctx)
  | Impl (name, params, methods) ->
      if List.length params > 0 then begin
        Hashtbl.add impl_templates name (params, methods);
        const_null (void_type ce_ctx)
      end
      else begin
        List.iter
          (fun (method_name, self_id, m_params, ret_ty, body) ->
            let mangled_name = name ^ "::" ^ method_name in
            let self_param = { param_name = self_id; ty = TNamed name } in
            let all_params = self_param :: m_params in
            ignore
              (codegen_stmt (DefFN (mangled_name, all_params, ret_ty, body))))
          methods;
        const_null (void_type ce_ctx)
      end
  | Raise e ->
      let err_msg = codegen_expr e in
      let ret_ty = !current_fn_ret_ty in
      let s1 =
        build_insertvalue (const_null ret_ty)
          (const_int (i1_type ce_ctx) 1)
          0 "err_flag" ce_builder
      in
      let s2 = build_insertvalue s1 err_msg 2 "err_msg" ce_builder in
      ignore (build_ret s2 ce_builder);
      const_null (void_type ce_ctx)

let optimize the_module =
  ignore (Llvm_all_backends.initialize ());
  let target_triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple target_triple in
  let target_machine =
    Llvm_target.TargetMachine.create ~triple:target_triple target
  in

  let pbo = Llvm_passbuilder.create_passbuilder_options () in

  begin match
    Llvm_passbuilder.run_passes the_module "default<O3>" target_machine pbo
  with
  | Ok () -> ()
  | Error msg -> Printf.eprintf "Optimization error: %s\n" msg
  end;

  Llvm_passbuilder.dispose_passbuilder_options pbo;
  the_module

let compile (stmts : stmt list) =
  List.iter (fun s -> ignore (codegen_stmt s)) stmts;
  optimize ce_module

let dump m =
  let module_string = string_of_llmodule m in
  print_endline module_string

open Llvm

let export binary_name the_module =
  ignore (Llvm_all_backends.initialize ());
  let target_triple = Llvm_target.Target.default_triple () in
  let target = Llvm_target.Target.by_triple target_triple in
  let machine =
    Llvm_target.TargetMachine.create ~triple:target_triple target
      ~reloc_mode:Llvm_target.RelocMode.PIC
  in

  let obj_filename = binary_name ^ ".o" in
  Llvm_target.TargetMachine.emit_to_file the_module
    Llvm_target.CodeGenFileType.ObjectFile obj_filename machine;

  let link_cmd = Printf.sprintf "cc %s -o %s" obj_filename binary_name in
  match Sys.command link_cmd with
  | 0 ->
      if Sys.file_exists obj_filename then Sys.remove obj_filename;
      ()
  | code ->
      Printf.eprintf "Linking failed with code %d\n" code;
      exit 1
