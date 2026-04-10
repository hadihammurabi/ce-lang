open Llvm
open Ce_parser.Ast

exception Error of string

let type_aliases : (string, types) Hashtbl.t = Hashtbl.create 10
let named_values : (string, llvalue * types) Hashtbl.t = Hashtbl.create 10
let function_types : (string, lltype) Hashtbl.t = Hashtbl.create 10

let struct_registry : (string, lltype * (string * int) list) Hashtbl.t =
  Hashtbl.create 10

let loop_exit_blocks : llbasicblock Stack.t = Stack.create ()

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
      | None -> raise (Error ("Undefined type: " ^ name)))
  | TStruct name ->
      let llty, _ = Hashtbl.find struct_registry name in
      llty
  | TUnknown -> raise (Error "Cannot compile unknown type")

let rec codegen_expr = function
  | Void -> const_null (void_type ce_ctx)
  | Int n -> const_int (i64_type ce_ctx) n
  | Float f -> const_float (double_type ce_ctx) f
  | Bool b -> const_int (i1_type ce_ctx) (if b then 1 else 0)
  | Char c -> const_int (i8_type ce_ctx) (Char.code c)
  | String s -> build_global_stringptr s "strtmp" ce_builder
  | ArrayAccess (name, index_expr) ->
      let array_ptr_val, array_ty =
        match Hashtbl.find_opt named_values name with
        | Some (v, ty) -> (v, ty)
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
      match Hashtbl.find_opt named_values name with
      | Some (v, ast_ty) -> build_load (llvm_type_of ast_ty) v name ce_builder
      | None -> raise (Error ("Unknown variable: " ^ name)))
  | Ref (Let name) ->
      let ptr_val, _ = Hashtbl.find named_values name in
      ptr_val
  | Ref _ -> raise (Error "Can only reference variables (e.g., &a)")
  | Deref (Let name) ->
      let ptr_to_ptr, ast_ty = Hashtbl.find named_values name in
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
      if type_of lv = double_type ce_ctx then
        build_fadd lv rv "faddtmp" ce_builder
      else build_add lv rv "addtmp" ce_builder
  | Sub (l, r) ->
      let lv = codegen_expr l in
      let rv = codegen_expr r in
      if type_of lv = double_type ce_ctx then
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
  | Call (fn_name, args) -> (
      let arg_vals = List.map codegen_expr args in
      match Builtin.get fn_name with
      | Some builtin_fn ->
          builtin_fn ce_ctx ce_module ce_builder fn_name arg_vals
      | None ->
          let callee =
            match lookup_function fn_name ce_module with
            | Some f -> f
            | None -> raise (Error ("Unknown function: " ^ fn_name))
          in
          let ft =
            match Hashtbl.find_opt function_types fn_name with
            | Some t -> t
            | None -> raise (Error ("Unknown function type for: " ^ fn_name))
          in
          let args_val = Array.of_list (List.map codegen_expr args) in
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

and codegen_stmt = function
  | Expr e ->
      ignore (codegen_expr e);
      const_null (void_type ce_ctx)
  | DefLet (name, _ismut, ty, expr) ->
      let init_val = codegen_expr expr in
      let the_function = block_parent (insertion_block ce_builder) in
      let ce_builder_alloca =
        builder_at ce_ctx (instr_begin (entry_block the_function))
      in

      let ll_ty = llvm_type_of ty in
      let alloca = build_alloca ll_ty name ce_builder_alloca in
      ignore (build_store init_val alloca ce_builder);

      Hashtbl.add named_values name (alloca, ty);
      alloca
  | DefType (name, underlying_ty) ->
      Hashtbl.add type_aliases name underlying_ty;
      const_null (void_type ce_ctx)
  | DefStruct (name, fields) ->
      let field_types =
        Array.of_list (List.map (fun f -> llvm_type_of f.ty) fields)
      in
      let struct_llty = named_struct_type ce_ctx name in
      struct_set_body struct_llty field_types false;

      let field_map = List.mapi (fun i f -> (f.field_name, i)) fields in
      Hashtbl.add struct_registry name (struct_llty, field_map);
      const_null (void_type ce_ctx)
  | Assign (name, expr) ->
      let val_ = codegen_expr expr in
      let var_ptr =
        match Hashtbl.find_opt named_values name with
        | Some (v, _) -> v
        | None -> raise (Error ("Variable not defined for assignment: " ^ name))
      in
      ignore (build_store val_ var_ptr ce_builder);
      val_
  | ArrayAssign (name, index_expr, val_expr) ->
      let array_ptr_val, array_ty =
        match Hashtbl.find_opt named_values name with
        | Some (v, ty) -> (v, ty)
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
      let ptr_to_ptr, ast_ty = Hashtbl.find named_values name in
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
      let param_types =
        Array.of_list (List.map (fun p -> llvm_type_of p.ty) params)
      in
      let ft = function_type (llvm_type_of ret_ty) param_types in
      Hashtbl.add function_types name ft;

      let f = declare_function name ft ce_module in
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
          Hashtbl.add named_values n (alloca, p_ty))
        (Llvm.params f);

      List.iter (fun s -> ignore (codegen_stmt s)) body;

      let current_bb = insertion_block ce_builder in
      (match block_terminator current_bb with
      | Some _ -> ()
      | None ->
          if ret_ty = TVoid then ignore (build_ret_void ce_builder)
          else
            raise
              (Error ("Function '" ^ name ^ "' is missing a return statement")));

      Hashtbl.clear named_values;
      Hashtbl.iter (fun k v -> Hashtbl.add named_values k v) old_named_values;
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
  | Return e -> build_ret (codegen_expr e) ce_builder
  | Import _ -> const_null (void_type ce_ctx)

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
