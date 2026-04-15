open Llvm
open Ce_parser.Ast
open State
open Utils
open Substitue

exception Error of string

let rec llvm_type_of = function
  | TInt | TUInt | TI32 | TU32 -> i32_type ce_ctx
  | TI8 | TU8 -> i8_type ce_ctx
  | TI16 | TU16 -> i16_type ce_ctx
  | TI64 | TU64 -> i64_type ce_ctx
  | TI128 | TU128 -> integer_type ce_ctx 128
  | TFloat | TF64 -> double_type ce_ctx
  | TF32 -> float_type ce_ctx
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
          | None -> (
              match Hashtbl.find_opt interface_registry name with
              | Some _ ->
                  struct_type ce_ctx
                    [| pointer_type ce_ctx; pointer_type ce_ctx |]
              | None -> raise (Error ("Undefined type: " ^ name)))))
  | TStruct name -> (
      try
        let llty, _ = Hashtbl.find struct_registry name in
        llty
      with Not_found -> raise (Error ("Unknown struct '" ^ name ^ "'")))
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

          let params, fields =
            try Hashtbl.find struct_templates name
            with Not_found ->
              raise
                (Error ("Cannot find generic struct template for '" ^ name ^ "'"))
          in
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
                  is_mut = f.is_mut;
                })
              fields
          in

          ignore
            (codegen_stmt (DefStruct (mangled_name, [], specialized_fields)));

          (match Hashtbl.find_opt impl_templates name with
          | Some (_, methods) ->
              let specialized_methods =
                List.map
                  (fun (m_name, self_id, is_ptr, m_params, ret_ty, body) ->
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
                      is_ptr,
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

and instantiate_generic_fn name targs =
  let mangled_name =
    name ^ "_" ^ String.concat "_" (List.map show_types targs)
  in

  if Hashtbl.mem function_types mangled_name then mangled_name
  else
    begin match Hashtbl.find_opt fn_templates name with
    | Some (tparams, fn_params, ret_ty, body) ->
        let type_map =
          List.map2 (fun (p_name, _) arg_ty -> (p_name, arg_ty)) tparams targs
        in
        let sub_params =
          List.map
            (fun p ->
              { param_name = p.param_name; ty = substitute_type type_map p.ty })
            fn_params
        in
        let sub_ret_ty = substitute_type type_map ret_ty in
        let sub_body = List.map (substitute_stmt type_map) body in
        let saved_bb =
          try Some (insertion_block ce_builder) with Not_found -> None
        in

        ignore
          (codegen_stmt
             (DefFN (mangled_name, [], sub_params, sub_ret_ty, sub_body)));
        (match saved_bb with
        | Some bb -> position_at_end bb ce_builder
        | None -> ());
        mangled_name
    | None -> raise (Error ("Undefined generic function: " ^ name))
    end

and infer_ast_type = function
  | Int _ -> TInt
  | Float _ -> TFloat
  | Bool _ -> TBool
  | String _ -> TString
  | Char _ -> TChar
  | Array (n, ty, _) -> TArray (n, ty)
  | Catch (_, _, ty, _) -> ty
  | Struct (name, targs, _) ->
      if targs = [] then TNamed name else TGenericInst (name, targs)
  | Add (l, _) | Sub (l, _) | Mul (l, _) | Div (l, _) | Mod (l, _) ->
      infer_ast_type l
  | Eq _ | Lt _ | Lte _ | Gt _ | Gte _ | And _ | Or _ -> TBool
  | Neg e -> infer_ast_type e
  | Ref e -> TPointer (infer_ast_type e)
  | Deref e -> ( match infer_ast_type e with TPointer t -> t | _ -> TUnknown)
  | Let name ->
      if not (String.contains name '.') then
        try
          let _, ty, _ = Hashtbl.find named_values name in
          ty
        with Not_found -> TUnknown
      else TUnknown
  | ArrayAccess (name, _) -> (
      try
        let _, ty, _ = Hashtbl.find named_values name in
        match ty with TArray (_, t) -> t | _ -> TUnknown
      with Not_found -> TUnknown)
  | Call (name, targs, _) -> (
      if String.ends_with ~suffix:".as" name && List.length targs = 1 then
        List.hd targs
      else if Hashtbl.mem fn_templates name then
        let tparams, _, ret_ty, _ = Hashtbl.find fn_templates name in
        if List.length tparams = List.length targs then
          let type_map =
            List.map2 (fun (p_name, _) arg_ty -> (p_name, arg_ty)) tparams targs
          in
          substitute_type type_map ret_ty
        else TUnknown
      else if String.contains name '.' then
        let last_dot = String.rindex name '.' in
        let base_path = String.sub name 0 last_dot in
        let method_name =
          String.sub name (last_dot + 1) (String.length name - last_dot - 1)
        in
        try
          let _, ast_ty, _ = Hashtbl.find named_values base_path in
          let actual_ty = match ast_ty with TPointer t -> t | t -> t in
          let s_name =
            match actual_ty with
            | TNamed n | TStruct n -> n
            | TGenericInst (n, arg_types) ->
                n ^ "_" ^ String.concat "_" (List.map show_types arg_types)
            | TInt | TI32 -> "int"
            | TFloat | TF64 -> "float"
            | TString -> "string"
            | TBool -> "bool"
            | TChar -> "char"
            | TI8 -> "i8"
            | TI16 -> "i16"
            | TI64 -> "i64"
            | TI128 -> "i128"
            | TUInt | TU32 -> "uint"
            | TU8 -> "u8"
            | TU16 -> "u16"
            | TU64 -> "u64"
            | TU128 -> "u128"
            | TF32 -> "f32"
            | _ -> raise Not_found
          in
          let mangled_name = s_name ^ "::" ^ method_name in
          let _, ret_ty = Hashtbl.find function_types mangled_name in
          ret_ty
        with Not_found -> (
          try
            let _, ret_ty = Hashtbl.find function_types name in
            ret_ty
          with Not_found -> TUnknown)
      else
        try
          let _, ret_ty = Hashtbl.find function_types name in
          ret_ty
        with Not_found -> TUnknown)
  | _ -> TUnknown

and codegen_expr = function
  | Void -> const_null (void_type ce_ctx)
  | Nil -> const_null (pointer_type ce_ctx)
  | Int n -> const_int (i32_type ce_ctx) n
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
            let is_ptr, base_struct_ast_ty =
              match ast_ty with TPointer t -> (true, t) | t -> (false, t)
            in
            let base_val_loaded =
              build_load (llvm_type_of ast_ty) v base_name ce_builder
            in

            let base_struct_val, base_struct_llty =
              if is_ptr then
                let ll_struct_ty = llvm_type_of base_struct_ast_ty in
                ( build_load ll_struct_ty base_val_loaded "auto_deref" ce_builder,
                  ll_struct_ty )
              else (base_val_loaded, llvm_type_of base_struct_ast_ty)
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
                                let _, idx, _ =
                                  List.find
                                    (fun (n, _, _) -> n = prop)
                                    field_map
                                in
                                let next_val =
                                  build_extractvalue current_val idx "proptmp"
                                    ce_builder
                                in
                                let next_ty =
                                  (struct_element_types current_ty).(idx)
                                in
                                extract next_val next_ty rest
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
            extract base_struct_val base_struct_llty props
        | None -> raise (Error ("Unknown variable: " ^ base_name))
      else
        match Hashtbl.find_opt named_values name with
        | Some (v, ast_ty, _) ->
            build_load (llvm_type_of ast_ty) v name ce_builder
        | None -> raise (Error ("Unknown variable: " ^ name)))
  | Ref (Let name) -> (
      try
        let ptr_val, _, _ = Hashtbl.find named_values name in
        ptr_val
      with Not_found ->
        raise (Error ("Cannot reference unknown variable: '" ^ name ^ "'")))
  | Ref _ -> raise (Error "Can only reference variables (e.g., &a)")
  | Deref (Let name) ->
      if String.contains name '.' then
        raise
          (Error
             ("Cannot use explicit dereference on struct fields directly. Try \
               just `" ^ name ^ "` as property accesses auto-dereference."));
      let ptr_to_ptr, ast_ty, _ =
        try Hashtbl.find named_values name
        with Not_found ->
          raise (Error ("Cannot dereference unknown variable: '" ^ name ^ "'"))
      in
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
      let lv, rv = (codegen_expr l, codegen_expr r) in
      if classify_type (type_of lv) = TypeKind.Pointer then
        let ptr_int = build_ptrtoint lv (i64_type ce_ctx) "pti" ce_builder in
        let rv_i64 = build_intcast rv (i64_type ce_ctx) "rv_i64" ce_builder in
        let added = build_add ptr_int rv_i64 "addptr" ce_builder in
        build_inttoptr added (type_of lv) "itp" ce_builder
      else if classify_type (type_of rv) = TypeKind.Pointer then
        let ptr_int = build_ptrtoint rv (i64_type ce_ctx) "pti" ce_builder in
        let lv_i64 = build_intcast lv (i64_type ce_ctx) "lv_i64" ce_builder in
        let added = build_add ptr_int lv_i64 "addptr" ce_builder in
        build_inttoptr added (type_of rv) "itp" ce_builder
      else build_numeric_op lv rv build_add build_fadd "addtmp"
  | Sub (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      if classify_type (type_of lv) = TypeKind.Pointer then
        let ptr_int = build_ptrtoint lv (i64_type ce_ctx) "pti" ce_builder in
        let rv_i64 = build_intcast rv (i64_type ce_ctx) "rv_i64" ce_builder in
        let subbed = build_sub ptr_int rv_i64 "subptr" ce_builder in
        build_inttoptr subbed (type_of lv) "itp" ce_builder
      else build_numeric_op lv rv build_sub build_fsub "subtmp"
  | Mul (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      build_numeric_op lv rv build_mul build_fmul "multmp"
  | Div (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      let is_unsigned =
        match infer_ast_type l with
        | TUInt | TU8 | TU16 | TU32 | TU64 | TU128 -> true
        | _ -> false
      in
      build_numeric_op lv rv
        (if is_unsigned then build_udiv else build_sdiv)
        build_fdiv "divtmp"
  | Mod (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      let is_unsigned =
        match infer_ast_type l with
        | TUInt | TU8 | TU16 | TU32 | TU64 | TU128 -> true
        | _ -> false
      in
      build_numeric_op lv rv
        (if is_unsigned then build_urem else build_srem)
        build_frem "modtmp"
  | Eq (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      build_numeric_op lv rv (build_icmp Icmp.Eq) (build_fcmp Fcmp.Oeq) "eqtmp"
  | Lt (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      let is_unsigned =
        match infer_ast_type l with
        | TUInt | TU8 | TU16 | TU32 | TU64 | TU128 -> true
        | _ -> false
      in
      let op = if is_unsigned then Icmp.Ult else Icmp.Slt in
      build_numeric_op lv rv (build_icmp op) (build_fcmp Fcmp.Olt) "lttmp"
  | Lte (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      let is_unsigned =
        match infer_ast_type l with
        | TUInt | TU8 | TU16 | TU32 | TU64 | TU128 -> true
        | _ -> false
      in
      let op = if is_unsigned then Icmp.Ule else Icmp.Sle in
      build_numeric_op lv rv (build_icmp op) (build_fcmp Fcmp.Ole) "ltetmp"
  | Gt (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      let is_unsigned =
        match infer_ast_type l with
        | TUInt | TU8 | TU16 | TU32 | TU64 | TU128 -> true
        | _ -> false
      in
      let op = if is_unsigned then Icmp.Ugt else Icmp.Sgt in
      build_numeric_op lv rv (build_icmp op) (build_fcmp Fcmp.Ogt) "gttmp"
  | Gte (l, r) ->
      let lv, rv = (codegen_expr l, codegen_expr r) in
      let is_unsigned =
        match infer_ast_type l with
        | TUInt | TU8 | TU16 | TU32 | TU64 | TU128 -> true
        | _ -> false
      in
      let op = if is_unsigned then Icmp.Uge else Icmp.Sge in
      build_numeric_op lv rv (build_icmp op) (build_fcmp Fcmp.Oge) "gtetmp"
  | And (l, r) ->
      build_and (codegen_expr l) (codegen_expr r) "andtmp" ce_builder
  | Or (l, r) -> build_or (codegen_expr l) (codegen_expr r) "ortmp" ce_builder
  | Neg e ->
      let v = codegen_expr e in
      if type_of v = double_type ce_ctx then build_fneg v "fnegtmp" ce_builder
      else build_neg v "negtmp" ce_builder
  | Call (name, targs, args) -> (
      if String.ends_with ~suffix:".as" name && List.length targs = 1 then begin
        let base_path = String.sub name 0 (String.length name - 3) in
        let target_ast_ty = List.hd targs in
        let target_ll_ty = llvm_type_of target_ast_ty in
        let result_ast_ty = TResult target_ast_ty in
        let result_ll_ty = llvm_type_of result_ast_ty in

        let self_val = codegen_expr (Let base_path) in
        let self_ll_ty = type_of self_val in

        let the_func = block_parent (insertion_block ce_builder) in
        let ok_bb = append_block ce_ctx "cast_ok" the_func in
        let err_bb = append_block ce_ctx "cast_err" the_func in
        let merge_bb = append_block ce_ctx "cast_merge" the_func in
        let is_interface =
          match classify_type self_ll_ty with
          | TypeKind.Struct ->
              let elems = struct_element_types self_ll_ty in
              Array.length elems = 2
              && elems.(0) = pointer_type ce_ctx
              && elems.(1) = pointer_type ce_ctx
          | _ -> false
        in

        if is_interface then begin
          let tag_ptr = build_extractvalue self_val 1 "tag_ptr" ce_builder in
          let tag_val =
            build_ptrtoint tag_ptr (i64_type ce_ctx) "tag_val" ce_builder
          in

          let expected_tag =
            match target_ast_ty with
            | TBool -> 3
            | TChar | TI8 | TU8 -> 5
            | TInt | TI16 | TI32 | TI64 | TI128 | TUInt | TU16 | TU32 | TU64
            | TU128 ->
                1
            | TFloat | TF32 | TF64 -> 2
            | TString | TPointer _ -> 4
            | _ -> 0
          in

          let is_match =
            build_icmp Icmp.Eq tag_val
              (const_int (i64_type ce_ctx) expected_tag)
              "is_match" ce_builder
          in
          ignore (build_cond_br is_match ok_bb err_bb ce_builder);

          position_at_end ok_bb ce_builder;
          let data_ptr = build_extractvalue self_val 0 "data_ptr" ce_builder in
          let loaded_val =
            build_load target_ll_ty data_ptr "loaded_val" ce_builder
          in

          let res_ok_0 =
            build_insertvalue (const_null result_ll_ty)
              (const_int (i1_type ce_ctx) 0)
              0 "res_ok0" ce_builder
          in
          let res_ok_1 =
            build_insertvalue res_ok_0 loaded_val 1 "res_ok1" ce_builder
          in
          let ok_end_bb = insertion_block ce_builder in
          ignore (build_br merge_bb ce_builder);

          position_at_end err_bb ce_builder;
          let err_msg =
            build_global_stringptr "Type cast failed" "err_msg" ce_builder
          in
          let res_err_0 =
            build_insertvalue (const_null result_ll_ty)
              (const_int (i1_type ce_ctx) 1)
              0 "res_err0" ce_builder
          in
          let res_err_1 =
            build_insertvalue res_err_0 err_msg 2 "res_err1" ce_builder
          in
          let err_end_bb = insertion_block ce_builder in
          ignore (build_br merge_bb ce_builder);

          position_at_end merge_bb ce_builder;
          build_phi
            [ (res_ok_1, ok_end_bb); (res_err_1, err_end_bb) ]
            "cast_res" ce_builder
        end
        else begin
          let casted_val =
            if self_ll_ty = target_ll_ty then self_val
            else if
              classify_type self_ll_ty = TypeKind.Integer
              && classify_type target_ll_ty = TypeKind.Integer
            then build_intcast self_val target_ll_ty "cast" ce_builder
            else if
              classify_type self_ll_ty = TypeKind.Double
              && target_ll_ty = float_type ce_ctx
            then build_fptrunc self_val target_ll_ty "cast" ce_builder
            else if
              classify_type self_ll_ty = TypeKind.Float
              && target_ll_ty = double_type ce_ctx
            then build_fpext self_val target_ll_ty "cast" ce_builder
            else if
              classify_type self_ll_ty = TypeKind.Integer
              && (classify_type target_ll_ty = TypeKind.Double
                 || classify_type target_ll_ty = TypeKind.Float)
            then build_sitofp self_val target_ll_ty "cast" ce_builder
            else if
              (classify_type self_ll_ty = TypeKind.Double
              || classify_type self_ll_ty = TypeKind.Float)
              && classify_type target_ll_ty = TypeKind.Integer
            then build_fptosi self_val target_ll_ty "cast" ce_builder
            else self_val
          in
          let res_ok_0 =
            build_insertvalue (const_null result_ll_ty)
              (const_int (i1_type ce_ctx) 0)
              0 "res_ok0" ce_builder
          in
          let res_ok_1 =
            build_insertvalue res_ok_0 casted_val 1 "res_ok1" ce_builder
          in
          res_ok_1
        end
      end
      else
        match Builtin.get name with
        | Some builtin_fn ->
            let arg_vals = List.map codegen_expr args in
            let targ_lltypes = List.map llvm_type_of targs in
            let arg_asts = List.map infer_ast_type args in
            builtin_fn ce_ctx ce_module ce_builder name arg_vals targ_lltypes
              arg_asts
        | None -> (
            let target_name =
              if targs = [] then name else instantiate_generic_fn name targs
            in
            match lookup_function name ce_module with
            | Some callee ->
                let ft, _ = Hashtbl.find function_types name in
                let expected_tys = param_types ft in
                let arg_vals =
                  List.mapi
                    (fun i arg ->
                      (coerce_value expected_tys.(i) (codegen_expr arg)) false)
                    args
                in
                let args_val = Array.of_list arg_vals in
                let call_name =
                  if return_type ft = void_type ce_ctx then "" else "calltmp"
                in
                build_call ft callee args_val call_name ce_builder
            | None -> (
                match lookup_function target_name ce_module with
                | Some callee ->
                    let ft, _ = Hashtbl.find function_types target_name in
                    let expected_tys = param_types ft in
                    let arg_vals =
                      List.mapi
                        (fun i arg ->
                          (coerce_value expected_tys.(i) (codegen_expr arg))
                            false)
                        args
                    in
                    let args_val = Array.of_list arg_vals in
                    let call_name =
                      if return_type ft = void_type ce_ctx then ""
                      else "calltmp"
                    in
                    build_call ft callee args_val call_name ce_builder
                | None ->
                    if Hashtbl.mem fn_templates name then
                      raise
                        (Error
                           ("Function '" ^ name
                          ^ "' is generic and requires type arguments (e.g., "
                          ^ name ^ "[int]())"))
                    else if String.contains name '.' then
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
                                   ("Unknown method '" ^ method_name
                                  ^ "' on struct '" ^ base_path ^ "'"))
                        in
                        let ft, _ = Hashtbl.find function_types mangled_name in
                        let expected_tys = param_types ft in
                        let arg_vals =
                          List.mapi
                            (fun i arg ->
                              (coerce_value expected_tys.(i) (codegen_expr arg))
                                false)
                            args
                        in
                        let args_val = Array.of_list arg_vals in
                        let call_name =
                          if return_type ft = void_type ce_ctx then ""
                          else "staticcalltmp"
                        in
                        build_call ft callee args_val call_name ce_builder
                      else
                        let self_val =
                          try codegen_expr (Let base_path)
                          with Error _ ->
                            raise
                              (Error
                                 ("Unknown function or method: '" ^ name ^ "'"))
                        in
                        let self_ty_llvm = type_of self_val in

                        let actual_struct_ty, is_self_ptr =
                          if classify_type self_ty_llvm = TypeKind.Pointer then
                            (element_type self_ty_llvm, true)
                          else (self_ty_llvm, false)
                        in

                        let self_ast_ty = infer_ast_type (Let base_path) in
                        let actual_ast_ty =
                          match self_ast_ty with TPointer t -> t | t -> t
                        in

                        let clean_name =
                          match actual_ast_ty with
                          | TNamed n | TStruct n -> n
                          | TInt | TI32 -> "int"
                          | TFloat | TF64 -> "float"
                          | TString -> "string"
                          | TBool -> "bool"
                          | TChar -> "char"
                          | TI8 -> "i8"
                          | TI16 -> "i16"
                          | TI64 -> "i64"
                          | TI128 -> "i128"
                          | TUInt | TU32 -> "uint"
                          | TU8 -> "u8"
                          | TU16 -> "u16"
                          | TU64 -> "u64"
                          | TU128 -> "u128"
                          | TF32 -> "f32"
                          | TGenericInst (n, arg_types) ->
                              n ^ "_"
                              ^ String.concat "_"
                                  (List.map show_types arg_types)
                          | _ -> (
                              match struct_name actual_struct_ty with
                              | Some s_name ->
                                  if String.starts_with ~prefix:"struct." s_name
                                  then
                                    String.sub s_name 7
                                      (String.length s_name - 7)
                                  else s_name
                              | None ->
                                  raise
                                    (Error
                                       ("Cannot call method '" ^ method_name
                                      ^ "' on a non-struct type")))
                        in

                        let mangled_name = clean_name ^ "::" ^ method_name in
                        let callee =
                          match lookup_function mangled_name ce_module with
                          | Some c -> c
                          | None ->
                              raise
                                (Error
                                   ("Unknown method '" ^ method_name
                                  ^ "' on type '" ^ clean_name ^ "'"))
                        in
                        let ft, _ = Hashtbl.find function_types mangled_name in
                        let expected_tys = param_types ft in

                        let expected_self_ty = expected_tys.(0) in
                        let coerced_self =
                          if classify_type expected_self_ty = TypeKind.Pointer
                          then
                            if is_self_ptr then self_val
                            else
                              let get_ptr_to_name path =
                                if String.contains path '.' then
                                  let parts = String.split_on_char '.' path in
                                  let base_name = List.hd parts in
                                  let v, ast_ty, _ =
                                    Hashtbl.find named_values base_name
                                  in
                                  resolve_property_ptr v (llvm_type_of ast_ty)
                                    (List.tl parts)
                                else
                                  let v, _, _ =
                                    Hashtbl.find named_values path
                                  in
                                  v
                              in
                              get_ptr_to_name base_path
                          else if is_self_ptr then
                            build_load actual_struct_ty self_val "deref_self"
                              ce_builder
                          else coerce_value expected_self_ty self_val false
                        in
                        let arg_vals =
                          List.mapi
                            (fun i arg ->
                              (coerce_value
                                 expected_tys.(i + 1)
                                 (codegen_expr arg))
                                false)
                            args
                        in
                        let all_args =
                          Array.of_list (coerced_self :: arg_vals)
                        in
                        let call_name =
                          if return_type ft = void_type ce_ctx then ""
                          else "methodcalltmp"
                        in
                        build_call ft callee all_args call_name ce_builder
                    else raise (Error ("Unknown function: " ^ name)))))
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
      let llty, field_map =
        try Hashtbl.find struct_registry mangled_name
        with Not_found ->
          raise (Error ("Cannot find struct '" ^ name ^ "' for instantiation"))
      in
      let alloc = build_alloca llty "structtmp" ce_builder in

      List.iter
        (fun (fname, fexpr) ->
          let _, fidx, _ = List.find (fun (n, _, _) -> n = fname) field_map in
          let fptr = build_struct_gep llty alloc fidx "fieldptr" ce_builder in
          let expected_ty = (struct_element_types llty).(fidx) in
          let raw_val = codegen_expr fexpr in
          let val_to_store = coerce_value expected_ty raw_val false in
          ignore (build_store val_to_store fptr ce_builder))
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

and coerce_value expected_ll_ty raw_val is_unsigned_target =
  let raw_ty = type_of raw_val in

  if is_unsigned_target && classify_type raw_ty = TypeKind.Integer then
    begin match int64_of_const raw_val with
    | Some v when v < 0L ->
        raise (Error "Cannot assign negative value to unsigned type")
    | Some _ -> ()
    | None ->
        let the_func = block_parent (insertion_block ce_builder) in
        let ok_bb = append_block ce_ctx "uint_ok" the_func in
        let err_bb = append_block ce_ctx "uint_err" the_func in
        let zero = const_int raw_ty 0 in
        let is_neg = build_icmp Icmp.Slt raw_val zero "is_neg" ce_builder in
        ignore (build_cond_br is_neg err_bb ok_bb ce_builder);

        position_at_end err_bb ce_builder;
        let printf_ty =
          var_arg_function_type (i32_type ce_ctx) [| pointer_type ce_ctx |]
        in
        let printf_fn =
          match lookup_function "printf" ce_module with
          | Some f -> f
          | None -> declare_function "printf" printf_ty ce_module
        in
        let err_fmt =
          build_global_stringptr
            "Runtime Error: Cannot assign negative value to unsigned type\n"
            "err_fmt" ce_builder
        in
        ignore (build_call printf_ty printf_fn [| err_fmt |] "p" ce_builder);
        let exit_ty = function_type (void_type ce_ctx) [| i32_type ce_ctx |] in
        let exit_fn =
          match lookup_function "exit" ce_module with
          | Some f -> f
          | None -> declare_function "exit" exit_ty ce_module
        in
        ignore
          (build_call exit_ty exit_fn
             [| const_int (i32_type ce_ctx) 1 |]
             "" ce_builder);
        ignore (build_unreachable ce_builder);

        position_at_end ok_bb ce_builder
    end;
  if raw_ty = expected_ll_ty then raw_val
  else if
    classify_type raw_ty = TypeKind.Integer
    && classify_type expected_ll_ty = TypeKind.Integer
  then build_intcast raw_val expected_ll_ty "int_coerce" ce_builder
  else if raw_ty = double_type ce_ctx && expected_ll_ty = float_type ce_ctx then
    build_fptrunc raw_val expected_ll_ty "float_trunc" ce_builder
  else if raw_ty = float_type ce_ctx && expected_ll_ty = double_type ce_ctx then
    build_fpext raw_val expected_ll_ty "float_ext" ce_builder
  else
    let is_result =
      match classify_type raw_ty with
      | TypeKind.Struct ->
          let elems = struct_element_types raw_ty in
          Array.length elems = 3
          && elems.(0) = i1_type ce_ctx
          && elems.(2) = pointer_type ce_ctx
      | _ -> false
    in
    if is_result then begin
      let is_err = build_extractvalue raw_val 0 "is_err" ce_builder in

      let the_func = block_parent (insertion_block ce_builder) in
      let err_bb = append_block ce_ctx "unwrap_err" the_func in
      let ok_bb = append_block ce_ctx "unwrap_ok" the_func in
      let merge_bb = append_block ce_ctx "unwrap_merge" the_func in

      ignore (build_cond_br is_err err_bb ok_bb ce_builder);

      position_at_end err_bb ce_builder;
      let err_msg = build_extractvalue raw_val 2 "err_msg" ce_builder in
      let printf_ty =
        var_arg_function_type (i32_type ce_ctx) [| pointer_type ce_ctx |]
      in
      let printf_fn =
        match lookup_function "printf" ce_module with
        | Some f -> f
        | None -> declare_function "printf" printf_ty ce_module
      in
      let err_fmt =
        build_global_stringptr "Uncaught Error: %s\n" "err_fmt" ce_builder
      in
      ignore
        (build_call printf_ty printf_fn [| err_fmt; err_msg |] "p" ce_builder);

      let exit_ty = function_type (void_type ce_ctx) [| i32_type ce_ctx |] in
      let exit_fn =
        match lookup_function "exit" ce_module with
        | Some f -> f
        | None -> declare_function "exit" exit_ty ce_module
      in
      ignore
        (build_call exit_ty exit_fn
           [| const_int (i32_type ce_ctx) 1 |]
           "" ce_builder);
      ignore (build_unreachable ce_builder);

      position_at_end ok_bb ce_builder;
      let ok_val = build_extractvalue raw_val 1 "ok_val" ce_builder in
      let final_val = coerce_value expected_ll_ty ok_val false in
      let final_ok_bb = insertion_block ce_builder in
      ignore (build_br merge_bb ce_builder);

      position_at_end merge_bb ce_builder;
      if expected_ll_ty = void_type ce_ctx then const_null (void_type ce_ctx)
      else build_phi [ (final_val, final_ok_bb) ] "unwrap_res" ce_builder
    end
    else
      let is_interface =
        match classify_type expected_ll_ty with
        | TypeKind.Struct ->
            let elems = struct_element_types expected_ll_ty in
            Array.length elems = 2
            && elems.(0) = pointer_type ce_ctx
            && elems.(1) = pointer_type ce_ctx
        | _ -> false
      in

      if is_interface then begin
        let actual_raw_val, actual_raw_ty =
          if classify_type raw_ty = TypeKind.Float then
            ( build_fpext raw_val (double_type ce_ctx) "box_fext" ce_builder,
              double_type ce_ctx )
          else (raw_val, raw_ty)
        in
        let malloc_val =
          build_malloc actual_raw_ty "autobox_malloc" ce_builder
        in
        ignore (build_store actual_raw_val malloc_val ce_builder);
        let ptr_ty = pointer_type ce_ctx in
        let data_ptr =
          build_bitcast malloc_val ptr_ty "autobox_data" ce_builder
        in
        let type_tag =
          match classify_type actual_raw_ty with
          | TypeKind.Integer ->
              let bw = integer_bitwidth actual_raw_ty in
              if bw = 1 then 3 else if bw = 8 then 5 else 1
          | TypeKind.Double -> 2
          | TypeKind.Pointer -> 4
          | _ -> 0
        in
        let tag_val = const_int (i64_type ce_ctx) type_tag in
        let vtable_ptr =
          build_inttoptr tag_val ptr_ty "autobox_tag" ce_builder
        in

        let box_0 =
          build_insertvalue
            (const_null expected_ll_ty)
            data_ptr 0 "autobox_d" ce_builder
        in
        build_insertvalue box_0 vtable_ptr 1 "autobox_v" ce_builder
      end
      else if
        classify_type expected_ll_ty = TypeKind.Pointer
        && classify_type raw_ty = TypeKind.Pointer
      then begin
        build_bitcast raw_val expected_ll_ty "ptr_cast" ce_builder
      end
      else raw_val

and codegen_stmt = function
  | Expr e ->
      let v = codegen_expr e in
      let ty = type_of v in
      let is_result =
        match classify_type ty with
        | TypeKind.Struct ->
            let elems = struct_element_types ty in
            Array.length elems = 3
            && elems.(0) = i1_type ce_ctx
            && elems.(2) = pointer_type ce_ctx
        | _ -> false
      in
      if is_result then
        ignore (coerce_value (struct_element_types ty).(1) v false);
      const_null (void_type ce_ctx)
  | DefLet (name, ismut, ty, expr_opt) ->
      let raw_val_opt, inferred_ty =
        match expr_opt with
        | Some expr ->
            let raw_val = codegen_expr expr in
            let deduced_ty =
              if ty = TUnknown then
                let inferred = infer_ast_type expr in
                if inferred = TUnknown then
                  raise
                    (Error
                       ("Cannot infer type for variable '" ^ name
                      ^ "'. Please specify the type explicitly."))
                else inferred
              else ty
            in
            (Some raw_val, deduced_ty)
        | None ->
            if ty = TUnknown then
              raise
                (Error
                   ("Cannot infer type for '" ^ name
                  ^ "' without initialization"));
            (None, ty)
      in

      let ll_ty = llvm_type_of inferred_ty in
      let is_unsigned =
        match inferred_ty with
        | TUInt | TU8 | TU16 | TU32 | TU64 | TU128 -> true
        | _ -> false
      in
      let init_val =
        match raw_val_opt with
        | Some raw_val -> coerce_value ll_ty raw_val is_unsigned
        | None -> const_null ll_ty
      in
      let the_function = block_parent (insertion_block ce_builder) in
      let ce_builder_alloca =
        builder_at ce_ctx (instr_begin (entry_block the_function))
      in

      let alloca = build_alloca ll_ty name ce_builder_alloca in

      ignore (build_store init_val alloca ce_builder);
      Hashtbl.add named_values name (alloca, inferred_ty, ismut);
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

        let field_map =
          List.mapi (fun i f -> (f.field_name, i, f.is_mut)) fields
        in
        Hashtbl.add struct_registry name (struct_llty, field_map);
        const_null (void_type ce_ctx)
      end
  | Assign (name, expr) ->
      let val_ = codegen_expr expr in
      let var_ptr, expected_ll_ty, is_unsigned =
        if String.contains name '.' then
          let parts = String.split_on_char '.' name in
          let base_name = List.hd parts in
          let v, ast_ty, _ = Hashtbl.find named_values base_name in

          let is_ptr, base_struct_ast_ty =
            match ast_ty with TPointer t -> (true, t) | t -> (false, t)
          in
          let base_struct_llty = llvm_type_of base_struct_ast_ty in

          let base_ptr =
            if is_ptr then
              build_load (llvm_type_of ast_ty) v "auto_deref_ptr" ce_builder
            else v
          in

          let rec get_gep ptr ty props =
            match props with
            | [] -> ptr
            | prop :: rest ->
                let s_name = Option.get (struct_name ty) in
                let clean_name =
                  if String.starts_with ~prefix:"struct." s_name then
                    String.sub s_name 7 (String.length s_name - 7)
                  else s_name
                in
                let _, field_map = Hashtbl.find struct_registry clean_name in
                let _, idx, _ =
                  List.find (fun (n, _, _) -> n = prop) field_map
                in
                let next_ptr =
                  build_struct_gep ty ptr idx "prop_ptr" ce_builder
                in
                let next_ty = (struct_element_types ty).(idx) in
                get_gep next_ptr next_ty rest
          in

          let rec get_ty ty props =
            match props with
            | [] -> ty
            | prop :: rest ->
                let s_name = Option.get (struct_name ty) in
                let clean_name =
                  if String.starts_with ~prefix:"struct." s_name then
                    String.sub s_name 7 (String.length s_name - 7)
                  else s_name
                in
                let _, field_map = Hashtbl.find struct_registry clean_name in
                let _, idx, is_field_mut =
                  List.find (fun (n, _, _) -> n = prop) field_map
                in
                if rest = [] && not is_field_mut then
                  raise
                    (Error
                       ("Cannot assign to immutable field '" ^ prop
                      ^ "' on struct '" ^ clean_name ^ "'"));
                get_ty (struct_element_types ty).(idx) rest
          in

          ( get_gep base_ptr base_struct_llty (List.tl parts),
            get_ty base_struct_llty (List.tl parts),
            false )
        else
          let v, ast_ty, ismut =
            try Hashtbl.find named_values name
            with Not_found ->
              raise (Error ("Unknown variable: '" ^ name ^ "'"))
          in
          if not ismut then
            raise (Error ("Cannot assign to immutable variable '" ^ name ^ "'"));

          let is_u =
            match ast_ty with
            | TUInt | TU8 | TU16 | TU32 | TU64 | TU128 -> true
            | _ -> false
          in
          (v, llvm_type_of ast_ty, is_u)
      in
      let val_to_store = coerce_value expected_ll_ty val_ is_unsigned in
      ignore (build_store val_ var_ptr ce_builder);
      val_to_store
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
      if String.contains name '.' then
        raise
          (Error
             ("Cannot use explicit dereference assignment on struct fields \
               directly. Try just `" ^ name
            ^ " = ...` as property accesses auto-dereference."));
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
  | DefFN (name, tparams, params, ret_ty, body) ->
      if List.length tparams > 0 then begin
        Hashtbl.add fn_templates name (tparams, params, ret_ty, body);
        const_null (void_type ce_ctx)
      end
      else begin
        let actual_name = if name = "main" then "__ce_main" else name in

        let was_res = !current_fn_is_res in
        let was_ret_ty = !current_fn_ret_ty in

        (current_fn_is_res := match ret_ty with TResult _ -> true | _ -> false);
        current_fn_ret_ty := llvm_type_of ret_ty;

        let param_types =
          Array.of_list (List.map (fun (p : param) -> llvm_type_of p.ty) params)
        in
        let ft = function_type (llvm_type_of ret_ty) param_types in
        Hashtbl.add function_types actual_name (ft, ret_ty);

        let f = declare_function actual_name ft ce_module in
        set_linkage Linkage.Internal f;

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

        List.iter
          (fun s ->
            if Option.is_none (block_terminator (insertion_block ce_builder))
            then ignore (codegen_stmt s))
          body;

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

          let call_name = if ret_ty = TVoid then "" else "main_call" in
          let call_res = build_call ft f [||] call_name c_builder in

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
      end
  | Block stmts ->
      List.iter
        (fun s ->
          if Option.is_none (block_terminator (insertion_block ce_builder)) then
            ignore (codegen_stmt s))
        stmts;
      const_null (void_type ce_ctx)
  | For stmts ->
      let the_function = block_parent (insertion_block ce_builder) in
      let loop_bb = append_block ce_ctx "loop" the_function in
      let after_bb = append_block ce_ctx "afterloop" the_function in

      ignore (build_br loop_bb ce_builder);
      position_at_end loop_bb ce_builder;

      Stack.push after_bb loop_exit_blocks;
      List.iter
        (fun s ->
          if Option.is_none (block_terminator (insertion_block ce_builder)) then
            ignore (codegen_stmt s))
        stmts;
      if Option.is_none (block_terminator (insertion_block ce_builder)) then
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
          (fun (method_name, self_id, is_ptr, m_params, ret_ty, body) ->
            let mangled_name = name ^ "::" ^ method_name in
            let base_ty =
              match name with
              | "int" -> TI32
              | "float" -> TF64
              | "uint" -> TU32
              | "string" -> TString
              | "bool" -> TBool
              | "char" -> TChar
              | "i8" -> TI8
              | "i16" -> TI16
              | "i64" -> TI64
              | "i128" -> TI128
              | "u8" -> TU8
              | "u16" -> TU16
              | "u64" -> TU64
              | "u128" -> TU128
              | "f32" -> TF32
              | _ -> TNamed name
            in

            let self_ty = if is_ptr then TPointer base_ty else base_ty in
            let self_param = { param_name = self_id; ty = self_ty } in
            let all_params = self_param :: m_params in
            ignore
              (codegen_stmt
                 (DefFN (mangled_name, [], all_params, ret_ty, body))))
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
  | DefInterface (name, sigs) ->
      Hashtbl.add interface_registry name sigs;
      const_null (void_type ce_ctx)

let optimize the_module =
  ignore (Llvm_all_backends.initialize ());
  let target_triple = Llvm_target.Target.default_triple () in
  let target_machine =
    Llvm_target.TargetMachine.create ~triple:target_triple
      (Llvm_target.Target.by_triple target_triple)
  in
  let pbo = Llvm_passbuilder.create_passbuilder_options () in
  ignore
    (Llvm_passbuilder.run_passes the_module "default<O3>" target_machine pbo);
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

  let link_cmd = Printf.sprintf "cc %s -lgc -o %s" obj_filename binary_name in
  match Sys.command link_cmd with
  | 0 ->
      if Sys.file_exists obj_filename then Sys.remove obj_filename;
      ()
  | code ->
      Printf.eprintf "Linking failed with code %d\n" code;
      exit 1
